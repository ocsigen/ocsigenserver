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
    let l = Ocsigen_base.Lib.Date.localtime t
    and u = Ocsigen_base.Lib.Date.gmtime t in
    let day_diff =
      if l.Unix.tm_year <> u.Unix.tm_year
      then compare l.Unix.tm_year u.Unix.tm_year
      else compare l.Unix.tm_yday u.Unix.tm_yday
    in
    (((day_diff * 24) + l.Unix.tm_hour - u.Unix.tm_hour) * 60)
    + l.Unix.tm_min - u.Unix.tm_min

  let time t =
    let tm = Ocsigen_base.Lib.Date.localtime t in
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

let handler ~ssl ~address ~port ~connector ~guard (flow, conn) request body =
  (* cohttp calls this once the headers of [request] have arrived. *)
  Connection_guard.set_phase guard Connection_guard.Request;
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
    let headers, ret_code, explicit_msg =
      match exn with
      | Ocsigen_http_error (cookies_to_set, code) ->
          let headers =
            Cookie.serialize cookies_to_set (Cohttp.Header.init ())
          in
          Some headers, code, None
      | Ocsigen_base.Ocsigen_stream.Interrupted
          Ocsigen_base.Ocsigen_stream.Already_read ->
          None, `Internal_server_error, None
      | Unix.Unix_error (Unix.EACCES, _, _) -> None, `Forbidden, None
      | Ext_http_error (code, msg, headers) -> headers, code, msg
      | Ocsigen_base.Lib.Ocsigen_Bad_Request -> None, `Bad_request, None
      | Ocsigen_base.Lib.Ocsigen_Request_too_long ->
          None, `Request_entity_too_large, None
      | Ocsigen_base.Lib.Input_is_too_large ->
          Logs.warn ~src:section (fun fmt ->
            fmt
              "Request body too large to be decoded in memory (see the <maxrequestbodysizeinmemory> and <netbuffersize> configuration options)");
          None, `Request_entity_too_large, None
      (* The body of the request did not arrive in time. The socket is shut
         down before this is sent (see [Connection_guard]), and the closing is
         logged then. The access log records the request with this status all
         the same, as nginx and Apache do. *)
      | Connection_guard.Timed_out -> None, `Request_timeout, None
      | exn ->
          Logs.err ~src:section (fun fmt ->
            fmt
              ("Error while handling request." ^^ "@\n%s")
              (Printexc.to_string exn));
          None, `Internal_server_error, None
    in
    let body =
      (* Never leak the OCaml exception (constructor names, file paths, ...)
         to the client: the exception detail stays in the logs (above). The
         client only gets the generic status reason phrase, or the explicit
         message an extension chose to expose through [Ext_http_error]. *)
      match explicit_msg with
      | Some msg -> msg
      | None ->
          Cohttp.Code.reason_phrase_of_code
            (Cohttp.Code.code_of_status (ret_code :> Cohttp.Code.status_code))
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
       let res, write_body = Response.to_response_expert response in
       (* Once the response is written, the connection waits for the next
          request. *)
       Lwt.return
         ( res
         , fun ic oc ->
             write_body ic oc >|= fun () ->
             Connection_guard.set_phase guard Connection_guard.Idle ))
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

(* The TLS context of a service. It requires TLS 1.2 at minimum (TLS 1.0 and
   1.1 are deprecated, RFC 8996) and lets the server choose the cipher. The
   <ciphers>, <dhfile> and <curve> entries of the <ssl> configuration are
   honoured. *)
let tls_context (crt, key, password) =
  (* A context for TLS 1.3 only, the protocols that accept all versions being
     deprecated, then opened to TLS 1.2. *)
  let ctx = Ssl.create_context Ssl.TLSv1_3 Ssl.Server_context in
  Ssl.set_min_protocol_version ctx Ssl.TLSv1_2;
  Ssl.honor_cipher_order ctx;
  (match Config.get_ssl_info () with
  | None -> ()
  | Some {Config.ssl_ciphers; ssl_dhfile; ssl_curve; _} ->
      Option.iter (Ssl.set_cipher_list ctx) ssl_ciphers;
      Option.iter (Ssl.init_dh_from_file ctx) ssl_dhfile;
      Option.iter (Ssl.init_ec_from_named_curve ctx) ssl_curve);
  Option.iter (Ssl.set_password_callback ctx) password;
  Ssl.use_certificate ctx crt key;
  ctx

(* The socket of a connection, and the address of its client, for the
   transports a service listens on. *)
let socket_of_flow = function
  | Conduit_lwt_unix.TCP {Conduit_lwt_unix.fd; ip; _} -> Some (fd, Some ip)
  | Conduit_lwt_unix.Domain_socket {Conduit_lwt_unix.fd; _} -> Some (fd, None)
  | Conduit_lwt_unix.Tunnel _ | Conduit_lwt_unix.Vchan _ -> None

let limits () =
  { Connection_guard.timeout = max 0 (Config.get_client_timeout ())
  ; keepalive_timeout = max 0 (Config.get_keepalive_timeout ())
  ; min_rate = max 0 (Config.get_min_data_rate ()) }

let client_to_string = function
  | Some ip -> Ipaddr.to_string ip
  | None -> "a local client"

(* A connection that exceeded its bounds is logged, and its socket shut down,
   so that its pending reads and writes fail at once, and so does anything the
   server would still try to send, such as a TLS close_notify. *)
let on_expiry ~fd ~client phase =
  Logs.info ~src:section (fun fmt ->
    fmt "Closing the connection of %s, which timed out %s"
      (client_to_string client)
      (match phase with
      | Connection_guard.Handshake -> "during the TLS handshake"
      | Connection_guard.Headers -> "while sending the headers of a request"
      | Connection_guard.Idle | Connection_guard.Request ->
          "while sending a request or reading a response"
      | Connection_guard.Closing -> "while the connection was ending"));
  try Lwt_unix.shutdown fd Unix.SHUTDOWN_ALL
  with Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()

(* The TLS session of a connection, or [None] if the handshake failed, as it
   does for clients that are not speaking TLS, or did not complete in time. *)
let accept_tls ~guard ctx fd =
  Lwt.catch
    (fun () ->
       Connection_guard.run guard (fun () -> Lwt_ssl.ssl_accept fd ctx)
       >|= Option.some)
    (function
      | Connection_guard.Timed_out -> Lwt.return_none
      | (Ssl.Accept_error _ | Unix.Unix_error _) as exn ->
          Logs.info ~src:section (fun fmt ->
            fmt "TLS handshake failed: %s" (Printexc.to_string exn));
          Lwt.return_none
      | exn -> Lwt.reraise exn)

(* Ends a connection: what remains of the response is sent, then [finish] is
   run, within the bounds of the closing phase. For TLS, [finish] sends a single
   close_notify alert: the close_notify of the client is not waited for (RFC
   8446, section 6.1), so that a client that never sends it cannot hold the
   connection. Nothing is sent on an expired connection, whose socket is shut
   down. The socket itself is closed by Conduit. *)
let end_connection ~guard oc ~finish =
  if Connection_guard.expired guard
  then Lwt.return_unit
  else (
    Connection_guard.set_phase guard Connection_guard.Closing;
    Lwt.catch
      (fun () ->
         Lwt_io.flush oc >>= fun () -> Connection_guard.run guard finish)
      (function
        | ( Connection_guard.Timed_out | Unix.Unix_error _ | Ssl.Write_error _
          | Ssl.Connection_error _ | Lwt_io.Channel_closed _ ) as exn ->
            Logs.debug ~src:section (fun fmt ->
              fmt "Could not end a connection: %s" (Printexc.to_string exn));
            Lwt.return_unit
        | exn -> Lwt.reraise exn))

(* Serves a connection whose bytes move through [read] and [write], plain or
   through TLS, each of them watched by [guard]. *)
let serve_guarded ~serve ~guard flow ~read ~write ~finish =
  let ic =
    Lwt_io.make ~mode:Lwt_io.input ~close:Lwt.return
      (Connection_guard.read guard read)
  and oc =
    Lwt_io.make ~mode:Lwt_io.output ~close:Lwt.return
      (Connection_guard.write guard write)
  in
  Lwt.finalize
    (fun () ->
       Lwt.catch
         (fun () -> serve ~guard flow ic oc)
         (function
           (* The expiry has been logged; or the connection was idle, and its
              closing is not worth a log. *)
           | Connection_guard.Timed_out -> Lwt.return_unit
           | exn -> Lwt.reraise exn))
    (fun () -> end_connection ~guard oc ~finish)

(* What [Cohttp_lwt_unix.Server.create] does, with its handler not exported,
   for an exception that escapes the serving of a connection. *)
let log_connection_error = function
  | Unix.Unix_error (error, func, arg) ->
      Logs.warn ~src:section (fun fmt ->
        fmt "Client connection error %s: %s(%S)" (Unix.error_message error) func
          arg)
  | exn ->
      Logs.err ~src:section (fun fmt ->
        fmt "Unhandled exception: %s" (Printexc.to_string exn))

let service ?ssl ~address ~port ~connector () =
  (* TLS is handled here rather than by Conduit, so that the handshake and the
     closing of a session are in the hands of the server. *)
  let tls =
    match address, ssl with
    | `Unix _, _ | _, None -> None
    | _, Some ssl -> Some (tls_context ssl)
  in
  (* We create a specific context for Conduit. *)
  let src =
    match address with
    | `Unix _ -> None
    | _ -> Some (Config.Socket_type.to_string address)
  in
  Conduit_lwt_unix.init ?src () >>= fun ctx ->
  Connection_guard.start ();
  (* We catch the INET_ADDR of the server *)
  let callback =
    let ssl = match ssl with Some _ -> true | None -> false in
    handler ~ssl ~address ~port ~connector
  in
  let serve ~guard flow ic oc =
    let spec =
      Cohttp_lwt_unix.Server.make_expert ~conn_closed
        ~callback:(callback ~guard) ()
    in
    Cohttp_lwt_unix.Server.callback spec flow
      (Cohttp_lwt_unix.Private.Input_channel.create ic)
      oc
  in
  let serve_socket ~guard flow fd =
    match tls with
    | None ->
        serve_guarded ~serve ~guard flow ~read:(Lwt_bytes.read fd)
          ~write:(Lwt_bytes.write fd) ~finish:Lwt.return
    | Some tls -> (
        accept_tls ~guard tls fd >>= function
        | None -> Lwt.return_unit
        | Some sock ->
            Connection_guard.set_phase guard Connection_guard.Headers;
            serve_guarded ~serve ~guard flow ~read:(Lwt_ssl.read_bytes sock)
              ~write:(Lwt_ssl.write_bytes sock) ~finish:(fun () ->
              Lwt_ssl.close_notify sock >|= fun (_ : bool) -> ()))
  in
  let mode =
    match address with
    | `Unix f -> `Unix_domain_socket (`File f)
    | _ -> `TCP (`Port port)
  in
  (* Serve each connection here rather than through
     [Cohttp_lwt_unix.Server.create], which does nothing more, so that its
     reads and writes can be watched. The channels made by Conduit are not
     used; Conduit closes the socket once the connection ends. *)
  Conduit_lwt_unix.serve ~stop ~on_exn:log_connection_error ~ctx ~mode
    (fun flow _ic _oc ->
       match socket_of_flow flow with
       | None ->
           Logs.err ~src:section (fun fmt ->
             fmt "Unsupported kind of connection");
           Lwt.return_unit
       | Some (fd, client) ->
           let limit = Config.get_max_connections_per_address () in
           if
             not
               (Option.fold ~none:true
                  ~some:(Connection_guard.Per_address.acquire ~limit)
                  client)
           then (
             Logs.info ~src:section (fun fmt ->
               fmt
                 "Refusing a connection from %s, which already has %d (see <maxconnectionsperaddress>)"
                 (client_to_string client) limit);
             Lwt.return_unit)
           else
             (* The address is released whatever happens from now on, even
                if watching the connection fails. *)
             Lwt.finalize
               (fun () ->
                  let guard =
                    Connection_guard.create (limits ())
                      (match tls with
                      | Some _ -> Connection_guard.Handshake
                      | None -> Connection_guard.Headers)
                      ~on_expiry:(on_expiry ~fd ~client)
                  in
                  Lwt.finalize
                    (fun () -> serve_socket ~guard flow fd)
                    (fun () ->
                       Connection_guard.release guard;
                       Lwt.return_unit))
               (fun () ->
                  Option.iter Connection_guard.Per_address.release client;
                  Lwt.return_unit))
  >>= fun () -> Lwt.return (Lwt.wakeup stop_wakener ())
