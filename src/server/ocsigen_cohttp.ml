open Lwt.Infix

let section = Lwt_log.Section.make "ocsigen:cohttp"

exception Ocsigen_http_error of
    Ocsigen_cookies.cookieset * Cohttp.Code.status

exception Ext_http_error of
    Cohttp.Code.status * string option * Http_headers.t option

(** print_request Print request for debug
    @param out_ch output for debug
    @param request Cohttp request *)

let print_request fmt request =
  let print_list print_data out_ch lst =
    let rec aux = function
      | [] -> ()
      | [ x ] -> print_data out_ch x
      | x :: r -> print_data out_ch x; aux r
    in aux lst
  in

  Format.fprintf fmt "%s [%s/%s]:\n"
    (Uri.to_string (Cohttp.Request.uri request))
    (Cohttp.Code.string_of_version request.version)
    (Cohttp.Code.string_of_method request.meth) ;

  Cohttp.Header.iter
    (fun key values ->
       Format.fprintf fmt "\t%s = %a\n" key
         (print_list Format.pp_print_string) values)
    request.headers

let waiters = Hashtbl.create 256

exception Ocsigen_Is_a_directory of (Ocsigen_request.t -> Neturl.url)

module Cookie = struct

  let serialize_cookie_raw path exp name c secure =
    Format.sprintf "%s=%s; path=/%s%s%s"
      name c
      (Ocsigen_lib.Url.string_of_url_path ~encode:true path)
      (if secure then "; secure" else "")
      (match exp with
       | Some s ->
         "; expires=" ^
         Netdate.format
           "%a, %d-%b-%Y %H:%M:%S GMT"
           (Netdate.create s)
       | None ->
         "")

  let serialize_cookies path table headers =
    Ocsigen_cookies.CookiesTable.fold
      (fun name c h ->
         let exp, v, secure = match c with
           | Ocsigen_cookies.OUnset -> (Some 0., "", false)
           | Ocsigen_cookies.OSet (t, v, secure) -> (t, v, secure)
         in
         Http_headers.add
           Http_headers.set_cookie
           (serialize_cookie_raw path exp name v secure)
           h)
      table
      headers

  let serialize cookies headers =
    Ocsigen_cookies.Cookies.fold serialize_cookies cookies headers

end

(* FIXME: secure *)
let make_cookies_header path exp name c secure =
  Format.sprintf "%s=%s%s%s" name c
    (*VVV encode = true? *)
    ("; path=/" ^ Ocsigen_lib.Url.string_of_url_path ~encode:true path)
    (* (if secure && slot.sl_ssl then "; secure" else "")^ *)
    "" ^
  (match exp with
   | Some s ->
     "; expires=" ^
     Netdate.format
       "%a, %d-%b-%Y %H:%M:%S GMT"
       (Netdate.create s)
   | None   -> "")

let make_cookies_headers path t hds =
  Ocsigen_cookies.CookiesTable.fold
    (fun name c h ->
       let exp, v, secure =
         match c with
         | Ocsigen_cookies.OUnset ->
           Some 0., "", false
         | Ocsigen_cookies.OSet (t, v, secure) ->
           t, v, secure
       in
       Cohttp.Header.add h
         Http_headers.(name_to_string set_cookie)
         (make_cookies_header path exp name v secure)
    )
    t
    hds

let handler ~address ~port ~connector (flow, conn) request body =

  Lwt_log.ign_info_f ~section
    "Receiving the request: %s"
    (Format.asprintf "%a" print_request request);

  let filenames = ref [] in
  let edn = Conduit_lwt_unix.endp_of_flow flow in
  let rec getsockname = function
    | `TCP (ip, port) ->
      Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip, port)
    | `Unix_domain_socket path ->
      Unix.ADDR_UNIX path
    | `TLS (_, edn) ->
      getsockname edn
    | `Unknown err ->
      raise (Failure ("resolution failed: " ^ err))
    | `Vchan_direct _ ->
      raise (Failure "VChan not supported")
    | `Vchan_domain_socket _ ->
      raise (Failure "VChan not supported")
  in

  let sockaddr = getsockname edn in
  let (waiter, wakener) = Lwt.wait () in
  Hashtbl.add waiters conn wakener;

  let handle_error exn =

    Lwt_log.ign_debug ~section ~exn "Got exception while handling request." ;

    let headers, ret_code = match exn with
      | Ocsigen_http_error (cookies_to_set, code) ->
        let headers =
          Cookie.serialize cookies_to_set (Cohttp.Header.init ())
        in
        Some headers, code
      | Ocsigen_stream.Interrupted Ocsigen_stream.Already_read ->
        None, `Internal_server_error
      | Unix.Unix_error (Unix.EACCES, _, _) ->
        None, `Forbidden
      | Ext_http_error (code, _, headers) ->
        headers, code
      | Ocsigen_lib.Ocsigen_Bad_Request ->
        None, `Bad_request
      | Neturl.Malformed_URL ->
        None, `Bad_request
      | Ocsigen_lib.Ocsigen_Request_too_long ->
        None, `Request_entity_too_large
      | exn ->
        Lwt_log.ign_error ~section ~exn "Error while handling request." ;
        None, `Internal_server_error
    in

    Lwt_log.ign_warning_f ~section "Returning error code %i."
      (Cohttp.Code.code_of_status (ret_code :> Cohttp.Code.status_code));

    let body =
      match ret_code with
      | `Not_found -> "Not Found"
      | _ -> Printexc.to_string exn in

    Cohttp_lwt_unix.Server.respond_error
      ?headers ~status:(ret_code :> Cohttp.Code.status_code) ~body ()
  in

  if !filenames <> [] then
    List.iter
      (fun a ->
         try
           Unix.unlink a
         with Unix.Unix_error _ as exn ->
           Lwt_log.ign_warning_f ~section ~exn
             "Error while removing file %s" a)
      !filenames;

  (* TODO: equivalent of Ocsigen_range *)

  let request =
    Ocsigen_request.make
      ~address ~port ~filenames ~sockaddr ~request ~body ~waiter
      ()

  in

  Lwt.catch
    (fun () ->
       connector request >>= fun { Ocsigen_response.a_response ;
                                   a_cookies ;
                                   a_body } ->

       let a_response =
         let headers =
           Ocsigen_cookies.Cookies.fold
             make_cookies_headers
             a_cookies
             (Cohttp.Response.headers a_response)
         in
         { a_response with Cohttp.Response.headers }
       in
       Lwt.return (a_response, a_body))

    (function
      | Ocsigen_Is_a_directory fun_request ->
        Cohttp_lwt_unix.Server.respond_redirect
          ~uri:
            (fun_request request
             |> Neturl.string_of_url
             |> Uri.of_string)
          ()
      | exn ->
        handle_error exn)

let conn_closed (flow, conn) =
  try
    let wakener = Hashtbl.find waiters conn in
    Lwt.wakeup wakener (); Hashtbl.remove waiters conn
  with Not_found -> ()

let stop, stop_wakener = Lwt.wait ()

let shutdown timeout =
  let process =
    match timeout with
    | Some f -> (fun () -> Lwt_unix.sleep f)
    | None -> (fun () -> Lwt.return ())
  in
  ignore (Lwt.pick [process (); stop] >>= fun () -> exit 0)

let number_of_client () = 0
let get_number_of_connected = number_of_client

let service ?ssl ~address ~port ~connector () =
  let tls_server_key = match ssl with
    | Some (crt, key, Some password) ->
      `TLS (`Crt_file_path crt,
            `Key_file_path key,
            `Password password)
    | Some (crt, key, None) ->
      `TLS (`Crt_file_path crt,
            `Key_file_path key,
            `No_password)
    | None -> `None
  in
  (* We create a specific context for Conduit and Cohttp. *)
  Conduit_lwt_unix.init
    ~src:(Ocsigen_socket.string_of_socket_type address)
    ~tls_server_key () >>= fun conduit_ctx ->
  Lwt.return (Cohttp_lwt_unix_net.init ~ctx:conduit_ctx ()) >>= fun ctx ->
  (* We catch the INET_ADDR of the server *)
  let callback =
    let address = Ocsigen_socket.to_inet_addr address in
    handler ~address  ~port ~connector
  in
  let config = Cohttp_lwt_unix.Server.make ~conn_closed ~callback () in
  let mode =
    match tls_server_key with
    | `None -> `TCP (`Port port)
    | `TLS (crt, key, pass) ->
      `OpenSSL (crt, key, pass, `Port port)
  in
  Cohttp_lwt_unix.Server.create ~stop ~ctx ~mode  config
  >>= fun () ->
  Lwt.return (Lwt.wakeup stop_wakener ())
