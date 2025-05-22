open Eio.Std

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

(* TODO: Use [_conn_sw] for nested concurrency *)
let handler
      ~ssl
      ~address
      ~port
      ~connector
      ((_conn_sw, eio_stream), conn)
      request
      body
  =
  let filenames = ref [] in
  let sockaddr =
    match eio_stream with
    | `Unix s -> Unix.ADDR_UNIX s
    | `Tcp (ip, port) ->
        Unix.ADDR_INET
          (Unix.inet_addr_of_string (ip : _ Eio.Net.Ipaddr.t :> string), port)
  in
  let connection_closed =
    try fst (Hashtbl.find connections conn)
    with Not_found ->
      let ((connection_closed, _) as p) = Promise.create () in
      Hashtbl.add connections conn p;
      incr_connected ();
      connection_closed
  in
  let handle_error exn =
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
            fmt "Error while handling request.@\n%a" Eio.Exn.pp exn);
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
  Fun.protect
    ~finally:(fun () ->
      if !filenames <> []
      then
        List.iter
          (fun a ->
             try Unix.unlink a
             with Unix.Unix_error _ as exn ->
               Logs.warn ~src:section (fun fmt ->
                 fmt "Error while removing file %s@\n%a" a Eio.Exn.pp exn))
          !filenames)
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
       let response =
         try connector request with
         | Ocsigen_is_dir fun_request ->
             let headers =
               fun_request request |> Uri.to_string
               |> Cohttp.Header.init_with "location"
             and status = `Moved_permanently in
             Ocsigen_response.respond_string ~headers ~status ~body:"" ()
         | exn -> handle_error exn
       in
       Ocsigen_response.to_response_expert response)

let conn_closed (_flow, conn) =
  try
    Promise.resolve (snd (Hashtbl.find connections conn)) ();
    Hashtbl.remove connections conn;
    decr_connected ()
  with Not_found -> ()

let stop, stop_wakener = Promise.create ()

let shutdown timeout =
  let process =
    match timeout with
    | Some f -> fun () -> Eio_unix.sleep f
    | None -> fun () -> ()
  in
  ignore
    (Fiber.any [process; (fun () -> Promise.await stop)];
     exit 0
     : unit Promise.t)

let service ?ssl ~address ~port ~connector ~on_error () =
  let tls_own_key =
    match ssl with
    | Some (crt, key, Some password) ->
        `TLS (`Crt_file_path crt, `Key_file_path key, `Password password)
    | Some (crt, key, None) ->
        `TLS (`Crt_file_path crt, `Key_file_path key, `No_password)
    | None -> `None
  in
  (* We catch the INET_ADDR of the server *)
  let callback =
    let ssl = match ssl with Some _ -> true | None -> false in
    handler ~ssl ~address ~port ~connector
  in
  let config = Cohttp_eio.Server.make_expert ~conn_closed ~callback () in
  let sockaddr =
    match address, tls_own_key with
    | (`Unix _ as a), _ -> a
    | `All, `None -> `Tcp (Eio.Net.Ipaddr.V4.any, port)
    | `IPv4 ip, `None ->
        `Tcp (Eio.Net.Ipaddr.of_raw (Unix.string_of_inet_addr ip), port)
    | _, `None -> `Tcp (Eio.Net.Ipaddr.V4.any, port)
    | _, `TLS (_crt, _key, _pass) -> failwith "TLS is not supported"
  in
  let sw = Stdlib.Option.get (Fiber.get Ocsigen_lib.current_switch) in
  let env = Stdlib.Option.get (Fiber.get Ocsigen_lib.env) in
  let listening_socket = Eio.Net.listen ~sw ~backlog:100 env#net sockaddr in
  Cohttp_eio.Server.run ~stop ~on_error listening_socket config;
  Promise.resolve stop_wakener ()
