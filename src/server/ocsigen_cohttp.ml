open Lwt.Infix

let section = Logs.Src.create "ocsigen:cohttp"

exception Ocsigen_http_error of Ocsigen_cookie_map.t * Cohttp.Code.status

exception
  Ext_http_error of Cohttp.Code.status * string option * Cohttp.Header.t option

(** print_request Print request for debug
    @param out_ch output for debug
    @param request Cohttp request *)

let _print_request fmt request =
  Format.fprintf fmt "%s [%s/%s]:\n"
    (Uri.to_string (Cohttp.Request.uri request))
    Cohttp.(Code.string_of_version (Request.version request))
    Cohttp.(Code.string_of_method (Request.meth request));
  Cohttp.Header.iter
    (fun key value -> Format.fprintf fmt "\t%s = %s\n" key value)
    (Cohttp.Request.headers request)

let connections = Hashtbl.create 256

let get_number_of_connected, incr_connected, decr_connected =
  (* TODO: Use Atomic once the 4.12 dependency is acceptable. *)
  let connected = ref 0 in
  (fun () -> !connected), (fun () -> incr connected), fun () -> decr connected

exception Ocsigen_is_dir of (Ocsigen_request.t -> Uri.t)

module Cookie = struct
  let serialize_cookie_raw path exp name c secure =
    Format.sprintf "%s=%s; path=/%s%s%s" name c
      (Ocsigen_lib.Url.string_of_url_path ~encode:true path)
      (if secure then "; secure" else "")
      (match exp with
      | Some s -> "; expires=" ^ Ocsigen_lib.Date.to_string s
      | None -> "")

  let serialize_cookies path =
    Ocsigen_cookie_map.Map_inner.fold @@ fun name c h ->
    let open Ocsigen_cookie_map in
    let exp, v, secure =
      match c with
      | OUnset -> Some 0., "", false
      | OSet (t, v, secure) -> t, v, secure
    in
    Cohttp.Header.add h
      Ocsigen_header.Name.(to_string set_cookie)
      (serialize_cookie_raw path exp name v secure)

  let serialize cookies headers =
    Ocsigen_cookie_map.Map_path.fold serialize_cookies cookies headers
end

let handler ~ssl ~address ~port ~connector (flow, conn) request body =
  let filenames = ref [] in
  let edn = Conduit_lwt_unix.endp_of_flow flow in
  let getsockname = function
    | `TCP (ip, port) | `TLS (_, `TCP (ip, port)) ->
        Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip, port)
    | `Unix_domain_socket path | `TLS (_, `Unix_domain_socket path) ->
        Unix.ADDR_UNIX path
    | _ -> raise (Failure "resolution failed")
  in
  let sockaddr = getsockname edn in
  let connection_closed =
    try fst (Hashtbl.find connections conn)
    with Not_found ->
      let ((connection_closed, _) as p) = Lwt.wait () in
      Hashtbl.add connections conn p;
      incr_connected ();
      connection_closed
  in
  let handle_error exn =
    Logs.debug ~src:section (fun fmt ->
      fmt
        ("Got exception while handling request." ^^ "@\n%s")
        (Printexc.to_string exn));
    let headers, ret_code =
      match exn with
      | Ocsigen_http_error (cookies_to_set, code) ->
          let headers =
            Cookie.serialize cookies_to_set (Cohttp.Header.init ())
          in
          Some headers, code
      | Ocsigen_stream.Interrupted Ocsigen_stream.Already_read ->
          None, `Internal_server_error
      | Unix.Unix_error (Unix.EACCES, _, _) -> None, `Forbidden
      | Ext_http_error (code, _, headers) -> headers, code
      | Ocsigen_lib.Ocsigen_Bad_Request -> None, `Bad_request
      | Ocsigen_lib.Ocsigen_Request_too_long -> None, `Request_entity_too_large
      | exn ->
          Logs.err ~src:section (fun fmt ->
            fmt
              ("Error while handling request." ^^ "@\n%s")
              (Printexc.to_string exn));
          None, `Internal_server_error
    in
    let body =
      match ret_code with
      | `Not_found -> "Not Found"
      | _ -> Printexc.to_string exn
    in
    Ocsigen_response.respond_error ?headers
      ~status:(ret_code :> Cohttp.Code.status_code)
      ~body ()
  in
  (* TODO: equivalent of Ocsigen_range *)
  let request =
    Ocsigen_request.make ~address ~port ~ssl ~filenames ~sockaddr ~body
      ~connection_closed request
  in
  Lwt.finalize
    (fun () ->
       Ocsigen_messages.accesslog
         (Format.sprintf "connection for %s from %s (%s)%s: %s"
            (match Ocsigen_request.host request with
            | None -> "<host not specified in the request>"
            | Some h -> h)
            (Ocsigen_request.remote_ip request)
            (Option.value ~default:""
               (Ocsigen_request.header request Ocsigen_header.Name.user_agent))
            (Option.fold ~none:""
               ~some:(fun s -> " X-Forwarded-For: " ^ s)
               (Ocsigen_request.header request
                  Ocsigen_header.Name.x_forwarded_for))
            (Uri.path (Ocsigen_request.uri request)));
       Lwt.catch
         (fun () -> connector request)
         (function
           | Ocsigen_is_dir fun_request ->
               let headers =
                 fun_request request |> Uri.to_string
                 |> Cohttp.Header.init_with "location"
               and status = `Moved_permanently in
               Lwt.return
                 (Ocsigen_response.respond_string ~headers ~status ~body:"" ())
           | exn -> Lwt.return (handle_error exn))
       >>= fun response ->
       Lwt.return (Ocsigen_response.to_response_expert response))
    (fun () ->
       if !filenames <> []
       then
         List.iter
           (fun a ->
              try Unix.unlink a
              with Unix.Unix_error _ as exn ->
                Logs.warn ~src:section (fun fmt ->
                  fmt
                    ("Error while removing file %s" ^^ "@\n%s")
                    a (Printexc.to_string exn)))
           !filenames;
       Lwt.return_unit)

let conn_closed (_flow, conn) =
  try
    Lwt.wakeup (snd (Hashtbl.find connections conn)) ();
    Hashtbl.remove connections conn;
    decr_connected ()
  with Not_found -> ()

let stop, stop_wakener = Lwt.wait ()

let shutdown timeout =
  let process =
    match timeout with
    | Some f -> fun () -> Lwt_unix.sleep f
    | None -> fun () -> Lwt.return ()
  in
  ignore (Lwt.pick [process (); stop] >>= fun () -> exit 0 : unit Lwt.t)

let service ?ssl ~address ~port ~connector () =
  let tls_own_key =
    match ssl with
    | Some (crt, key, Some password) ->
        `TLS (`Crt_file_path crt, `Key_file_path key, `Password password)
    | Some (crt, key, None) ->
        `TLS (`Crt_file_path crt, `Key_file_path key, `No_password)
    | None -> `None
  in
  (* We create a specific context for Conduit and Cohttp. *)
  let src =
    match address with
    | `Unix _ -> None
    | _ -> Some (Ocsigen_config.Socket_type.to_string address)
  in
  Conduit_lwt_unix.init ?src ~tls_own_key () >>= fun conduit_ctx ->
  Lwt.return (Cohttp_lwt_unix.Net.init ~ctx:conduit_ctx ()) >>= fun ctx ->
  (* We catch the INET_ADDR of the server *)
  let callback =
    let ssl = match ssl with Some _ -> true | None -> false in
    handler ~ssl ~address ~port ~connector
  in
  let config = Cohttp_lwt_unix.Server.make_expert ~conn_closed ~callback () in
  let mode =
    match address, tls_own_key with
    | `Unix f, _ -> `Unix_domain_socket (`File f)
    | _, `None -> `TCP (`Port port)
    | _, `TLS (crt, key, pass) -> `OpenSSL (crt, key, pass, `Port port)
  in
  Cohttp_lwt_unix.Server.create ~stop ~ctx ~mode config >>= fun () ->
  Lwt.return (Lwt.wakeup stop_wakener ())
