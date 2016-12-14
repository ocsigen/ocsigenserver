open Lwt.Infix

let section = Lwt_log.Section.make "ocsigen:cohttp"

exception Ocsigen_unsupported_media
exception Ocsigen_http_error of (Ocsigen_cookies.cookieset * int)

module Connection = struct
  exception Lost_connection of exn
  exception Aborted
  exception Timeout
  exception Keepalive_timeout
  exception Connection_closed
end

(** print_cohttp_request Print request for debug
 * @param out_ch output for debug
 * @param request Cohttp request *)

let print_cohttp_request fmt request =
  let print_list print_data out_ch lst =
    let rec aux = function
      | [] -> ()
      | [ x ] -> print_data out_ch x
      | x :: r -> print_data out_ch x; aux r
    in aux lst
  in

  let open Cohttp.Request in

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

type request = {
  r_address       : Unix.inet_addr ;
  r_port          : int ;
  r_filenames     : string list ref;
  r_sockaddr      : Lwt_unix.sockaddr ;
  r_request       : Cohttp.Request.t ;
  r_body          : Cohttp_lwt_body.t ;
  r_waiter        : unit Lwt.t ;
  mutable r_tries : int
}

type result = {
  r_response : Cohttp.Response.t ;
  r_body     : Cohttp_lwt_body.t ;
  r_cookies  : Ocsigen_cookies.cookieset
}

let result_of_cohttp
    ?(cookies = Ocsigen_cookies.empty_cookieset)
    (r_response, r_body) =
  { r_response ; r_body ; r_cookies = cookies }

let path_of_request {r_request} =
  Cohttp.Request.uri r_request
  |> Uri.path
  |> Ocsigen_lib.Url.split_path

let incr_tries r = r.r_tries <- r.r_tries + 1

let tries {r_tries} = r_tries

exception Ocsigen_Is_a_directory of (request -> Neturl.url)

let handler ~address ~port ~connector (flow, conn) request body =

  Lwt_log.ign_info_f ~section
    "Receiving the request: %s"
    (Format.asprintf "%a" print_cohttp_request request)
  ;

  let filenames = ref [] in
  let edn = Conduit_lwt_unix.endp_of_flow flow in
  let rec getsockname = function
    | `TCP (ip, port) ->
      Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip, port)
    | `Unix_domain_socket path ->
      Unix.ADDR_UNIX path
    | `TLS (_, edn) -> getsockname edn
    | `Unknown err -> raise (Failure ("resolution failed: " ^ err))
    | `Vchan_direct _ -> raise (Failure "VChan not supported")
    | `Vchan_domain_socket _ -> raise (Failure "VChan not supported")
  in

  let sockaddr = getsockname edn in
  let (waiter, wakener) = Lwt.wait () in
  Hashtbl.add waiters conn wakener;

  let handle_error exn =

    Lwt_log.ign_debug ~section ~exn "Got exception while handling request." ;

    let headers, ret_code = match exn with
      | Ocsigen_http_error (cookies_to_set, code) ->
        let headers =
          To_cohttp.Cookie.serialize cookies_to_set (Cohttp.Header.init ())
        in
        Some headers, code
      | Ocsigen_stream.Interrupted Ocsigen_stream.Already_read ->
        None, 500
      | Unix.Unix_error (Unix.EACCES, _, _) ->
        None, 403
      | Ocsigen_http_frame.Http_error.Http_exception (code, _, headers) ->
        headers, code
      | Ocsigen_lib.Ocsigen_Bad_Request ->
        None, 400
      | Ocsigen_unsupported_media ->
        None, 415
      | Neturl.Malformed_URL ->
        None, 400
      | Ocsigen_lib.Ocsigen_Request_too_long ->
        None, 413
      | exn ->
        Lwt_log.ign_error ~section ~exn "Error while handling request." ;
        None, 500
    in

    Lwt_log.ign_warning_f ~section "Returning error code %i." ret_code ;

    let body =
      match ret_code with
      | 404 -> "Not Found"
      | _ -> Printexc.to_string exn in

    Cohttp_lwt_unix.Server.respond_error
      ?headers
      ~status:(Cohttp.Code.status_of_code ret_code)
      ~body ()
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

  connector {
    r_address   = address ;
    r_port      = port ;
    r_filenames = filenames ;
    r_sockaddr  = sockaddr;
    r_request   = request;
    r_body      = body;
    r_waiter    = waiter;
    r_tries     = 0
  } >>= fun { r_response ; r_body } ->

  (* TODO: handle cookies *)

  Lwt.return (r_response, r_body)

let conn_closed (flow, conn) =
  try let wakener = Hashtbl.find waiters conn in
      Lwt.wakeup wakener (); Hashtbl.remove waiters conn
  with Not_found -> ()

let stop, stop_wakener = Lwt.wait ()

let shutdown_server timeout =
  let process = match timeout with
    | Some f -> (fun () -> Lwt_unix.sleep f)
    | None -> (fun () -> Lwt.return ())
  in ignore
    begin
      (Lwt.pick [process (); stop])
      >>= fun () -> exit 0
    (* XXX: actually, deadlock with Lwt, cf. Lwt#48 *)
    end

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
