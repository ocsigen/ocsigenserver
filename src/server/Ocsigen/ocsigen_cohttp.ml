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

exception Ocsigen_is_dir of (Request.t -> Uri.t)

module Cookie = struct
  let serialize_cookie_raw path exp name c secure =
    Format.sprintf "%s=%s; path=/%s%s%s" name c
      (Ocsigen_base.Lib.Url.string_of_url_path ~encode:true path)
      (if secure then "; secure" else "")
      (match exp with
      | Some s -> "; expires=" ^ Ocsigen_base.Lib.Date.to_string s
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
      Ocsigen_http.Header.Name.(to_string set_cookie)
      (serialize_cookie_raw path exp name v secure)

  let serialize cookies headers =
    Ocsigen_cookie_map.Map_path.fold serialize_cookies cookies headers
end

(* Access log in the Apache/nginx Combined Log Format:
   %h %l %u %t "%r" %>s %b "%{Referer}i" "%{User-agent}i"
   https://httpd.apache.org/docs/current/logs.html#combined *)
module Access_log = struct
  (* Offset of local time from UTC, in minutes, robust across day boundaries. *)
  let timezone_offset t =
    let l = Unix.localtime t and u = Unix.gmtime t in
    let day_diff =
      if l.Unix.tm_year <> u.Unix.tm_year
      then compare l.Unix.tm_year u.Unix.tm_year
      else compare l.Unix.tm_yday u.Unix.tm_yday
    in
    (((day_diff * 24) + l.Unix.tm_hour - u.Unix.tm_hour) * 60)
    + l.Unix.tm_min - u.Unix.tm_min

  let time t =
    let tm = Unix.localtime t in
    let off = timezone_offset t in
    let sign = if off < 0 then '-' else '+' in
    let off = abs off in
    Printf.sprintf "[%02d/%s/%04d:%02d:%02d:%02d %c%02d%02d]" tm.Unix.tm_mday
      (Ocsigen_base.Lib.Date.name_of_month tm.Unix.tm_mon)
      (1900 + tm.Unix.tm_year) tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
      sign (off / 60) (off mod 60)

  (* Escape characters that would break the format or allow log injection in
     attacker-controlled fields (request line, Referer, User-Agent). *)
  let escape s =
    let buf = Buffer.create (String.length s) in
    String.iter
      (fun c ->
         match c with
         | '"' -> Buffer.add_string buf "\\\""
         | '\\' -> Buffer.add_string buf "\\\\"
         | '\n' -> Buffer.add_string buf "\\n"
         | '\r' -> Buffer.add_string buf "\\r"
         | '\t' -> Buffer.add_string buf "\\t"
         | c when Char.code c < 0x20 || Char.code c = 0x7f ->
             Buffer.add_string buf (Printf.sprintf "\\x%02x" (Char.code c))
         | c -> Buffer.add_char buf c)
      s;
    Buffer.contents buf

  let line request response =
    let quoted s = "\"" ^ escape s ^ "\"" in
    let header name =
      match Request.header request name with
      | Some s -> quoted s
      | None -> "\"-\""
    in
    let user =
      match Request.remote_user request with
      | Some u when u <> "" -> escape u
      | _ -> "-"
    in
    let request_line =
      (* string_of_version already yields "HTTP/1.1". *)
      Printf.sprintf "%s %s %s"
        (Cohttp.Code.string_of_method (Request.meth request))
        (Uri.path_and_query (Request.uri request))
        (Cohttp.Code.string_of_version (Request.version request))
    in
    let status =
      Cohttp.Code.code_of_status
        (Response.status response :> Cohttp.Code.status_code)
    in
    (* The response size is carried by the body transfer encoding, not a
       header: known for fixed-length bodies (e.g. static files), "-" when
       streamed with an unknown length. *)
    let bytes =
      match Response.Body.transfer_encoding (Response.body response) with
      | Cohttp.Transfer.Fixed n -> Int64.to_string n
      | Cohttp.Transfer.Chunked | Cohttp.Transfer.Unknown -> "-"
    in
    Printf.sprintf "%s - %s %s %s %d %s %s %s"
      (Request.client_ip_to_string request)
      user
      (time (Request.timeofday request))
      (quoted request_line) status bytes
      (header Ocsigen_http.Header.Name.referer)
      (header Ocsigen_http.Header.Name.user_agent)
end

let handler ~ssl ~address ~port ~connector (flow, conn) request body =
  let filenames = ref [] in
  let edn = Conduit_lwt_unix.endp_of_flow flow in
  let client_conn =
    match edn with
    | `TCP (ip, port) | `TLS (_, `TCP (ip, port)) -> `Inet (ip, port)
    | `Unix_domain_socket path | `TLS (_, `Unix_domain_socket path) ->
        `Unix path
    | _ -> `Unknown
  in
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
      | Ocsigen_base.Ocsigen_stream.Interrupted
          Ocsigen_base.Ocsigen_stream.Already_read ->
          None, `Internal_server_error
      | Unix.Unix_error (Unix.EACCES, _, _) -> None, `Forbidden
      | Ext_http_error (code, _, headers) -> headers, code
      | Ocsigen_base.Lib.Ocsigen_Bad_Request -> None, `Bad_request
      | Ocsigen_base.Lib.Ocsigen_Request_too_long ->
          None, `Request_entity_too_large
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
    Response.respond_error ?headers
      ~status:(ret_code :> Cohttp.Code.status_code)
      ~body ()
  in
  (* TODO: equivalent of Ocsigen_range *)
  let request =
    Request.make ~address ~port ~ssl ~filenames ~client_conn ~body
      ~connection_closed request
  in
  Lwt.finalize
    (fun () ->
       Lwt.catch
         (fun () -> connector request)
         (function
           | Ocsigen_is_dir fun_request ->
               let headers =
                 fun_request request |> Uri.to_string
                 |> Cohttp.Header.init_with "location"
               and status = `Moved_permanently in
               Lwt.return (Response.respond_string ~headers ~status ~body:"" ())
           | exn -> Lwt.return (handle_error exn))
       >>= fun response ->
       Messages.accesslog (Access_log.line request response);
       Lwt.return (Response.to_response_expert response))
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
    | _ -> Some (Config.Socket_type.to_string address)
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
