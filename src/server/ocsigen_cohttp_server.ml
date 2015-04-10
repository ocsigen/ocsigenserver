open Lwt
open Ocsigen_socket
open Ocsigen_lib
open Ocsigen_request_info
open Ocsigen_http_frame
open Ocsigen_headers
open Ocsigen_config
open Ocsigen_cookies
open Ocsigen_generate
open Lazy
open Cohttp
open Cohttp_lwt_unix

let section = Lwt_log.Section.make "ocsigen:cohttp"

module RI = Ocsigen_request_info (* An alias convenient for accessor *)

exception Ocsigen_Is_a_directory of (Ocsigen_request_info.request_info -> Neturl.url)
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
    (Uri.to_string request.uri)
    (Cohttp.Code.string_of_version request.version)
    (Cohttp.Code.string_of_method request.meth) ;

  Cohttp.Header.iter
    (fun key values ->
       Format.fprintf fmt "\t%s = %a\n" key
         (print_list Format.pp_print_string) values)
    request.headers

let waiters = Hashtbl.create 256

let handler ~address ~port ~extensions_connector (flow, conn) request body =

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
      | Unix.Unix_error (Unix.EACCES, _, _)
      | Ocsigen_upload_forbidden ->
        None, 403
      | Http_error.Http_exception (code, _, headers) ->
        headers, code
      | Ocsigen_Bad_Request ->
        None, 400
      | Ocsigen_unsupported_media ->
        None, 415
      | Neturl.Malformed_URL ->
        None, 400
      | Ocsigen_Request_too_long ->
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

    Server.respond_error
      ?headers
      ~status:(Cohttp.Code.status_of_code ret_code)
      ~body ()
  in
  Lwt.finalize (fun () ->
      Lwt.try_bind
        (fun () -> Ocsigen_generate.of_cohttp_request
            ~address
            ~port
            filenames
            sockaddr
            request
            body
            waiter)
        (fun ri ->
           Lwt_log.ign_debug_f ~section
             "Generating the Ocsigen request info: %s"
             (Format.asprintf "%a" Ocsigen_request_info.pp_request_info ri) ;
           Lwt.try_bind
             (extensions_connector ri)
             (fun res ->
                Ocsigen_range.compute_range ri res
                >|= To_cohttp.to_response_and_body)
             (function
               | Ocsigen_Is_a_directory fun_request ->
                 Server.respond_redirect
                   ~uri:(Uri.of_string @@ Neturl.string_of_url @@ fun_request ri) ()
               | exn -> handle_error exn))
        handle_error
    )
    (fun () ->
       if !filenames <> []
       then List.iter (fun a ->
           try Unix.unlink a
           with Unix.Unix_error _ as exn ->
             Lwt_log.ign_warning_f ~section ~exn
               "Error while removing file %s" a)
           !filenames; Lwt.return ())

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

(* Avoid duplicate code between server_tls_config and tls_server_key type.
 * It's conversion type specificaly for a TLS server. *)
let server_tls_config_of_tls_server_key
  : Conduit_lwt_unix.tls_server_key ->
    (int -> Conduit_lwt_unix.server_tls_config) =
  function `None -> assert false (* specific for a TLS server *)
         | `TLS (crt, key, pass) ->
           fun port -> (crt, key, pass, `Port port)

(* An http result [res] frame has been computed. Depending on
   the If-(None-)?Match and If-(Un)?Modified-Since headers of [ri],
   we return this frame, a 304: Not-Modified, or a 412: Precondition Failed.
   See RFC 2616, sections 14.24, 14.25, 14.26, 14.28 and 13.3.4
*)
let handle_result_frame ri res =
  (* Subfonctions to handle each header separately *)
  let if_unmodified_since unmodified_since = (* Section 14.28 *)
    if (Result.code res = 412 ||
        (200 <= Result.code res && Result.code res < 300)) then
      match Result.lastmodified res with
      | Some r ->
        if r <= unmodified_since then
          `Ignore_header
        else
          `Precondition_failed
      | None -> `Ignore_header
    else
      `Ignore_header

  and if_modified_since modified_since = (* Section 14.25 *)
    if Result.code res = 200 then
      match Result.lastmodified res with
      | Some r ->
        if r <= modified_since then
          `Unmodified
        else
          `Ignore_header
      | _ -> `Ignore_header
    else
      `Ignore_header

  and if_none_match if_none_match = (* Section 14.26 *)
    if (Result.code res = 412 ||
        (200 <= Result.code res && Result.code res < 300)) then
      match Result.etag res with
      | None   -> `Ignore_header
      | Some e ->
        if List.mem e if_none_match then
          if (Ocsigen_request_info.meth ri) = Http_header.GET ||
             (Ocsigen_request_info.meth ri) = Http_header.HEAD then
            `Unmodified
          else
            `Precondition_failed
        else
          `Ignore_header_and_ModifiedSince
    else
      `Ignore_header

  and if_match if_match = (* Section 14.24 *)
    if (Result.code res = 412 ||
        (200 <= Result.code res && Result.code res < 300)) then
      match Result.etag res with
      | None   -> `Precondition_failed
      | Some e ->
        if List.mem e if_match then
          `Ignore_header
        else
          `Precondition_failed
    else
      `Ignore_header

  in

  let handle_header f h = match h with
    | None -> `No_header
    | Some h -> f h
  in

  (* Main code *)
  let r =
    (* For the cases unspecified with RFC2616. we follow more or less
       the order used by Apache. See the function
       modules/http/http_protocol.c/ap_meets_conditions in the Apache
       source *)
    match handle_header if_match (Ocsigen_request_info.ifmatch ri) with
    | `Precondition_failed -> `Precondition_failed
    | `No_header | `Ignore_header ->
      match handle_header if_unmodified_since
              (Ocsigen_request_info.ifunmodifiedsince ri) with
      | `Precondition_failed -> `Precondition_failed
      | `No_header | `Ignore_header ->
        match handle_header if_none_match
                (Ocsigen_request_info.ifnonematch ri) with
        | `Precondition_failed -> `Precondition_failed
        | `Ignore_header_and_ModifiedSince -> `Std
        | `Unmodified | `No_header as r1 ->
          (match handle_header if_modified_since
                   (Ocsigen_request_info.ifmodifiedsince ri) with
          | `Unmodified | `No_header as r2 ->
            if r1 = `No_header && r2 = `No_header then
              `Std
            else
              `Unmodified
          | `Ignore_header -> `Std)
        | `Ignore_header ->
          (* We cannot return a 304, so there is no need to consult
             if_modified_since *)
          `Std
  in
  match r with
  | `Unmodified ->
    Lwt_log.ign_info ~section "Sending 304 Not modified";
    Ocsigen_stream.finalize (fst (Result.stream res)) `Success >>= fun () ->
    let headers =
      let keep h headers =
        try
          Http_headers.add h (Http_headers.find h (Result.headers res)) headers
        with Not_found ->
          headers
      in
      Http_headers.(keep cache_control (keep expires empty))
    in
    Result.update (Ocsigen_http_frame.Result.empty ())
      ~code:304  (* Not modified *)
      ~lastmodified:(Result.lastmodified res)
      ~etag:(Result.etag res)
      ~headers ()
    |> return
  | `Precondition_failed ->
    Lwt_log.ign_info ~section
      "Sending 412 Precondition Failed (conditional headers)";
    Ocsigen_stream.finalize (fst (Result.stream res)) `Success >|= fun () ->
    Result.update (Ocsigen_http_frame.Result.empty ())
      ~code:412 (* Precondition failed *) ()
  | `Std ->
    Ocsigen_range.compute_range ri res

let service ?ssl ~address ~port ~connector () =
  let connector ri () =
    connector ri () >>= fun res ->
    handle_result_frame ri res in
  let callback = handler ~address ~port ~extensions_connector:connector in
  let config = Server.make ~conn_closed ~callback () in
  (match ssl with
   | None -> Server.create ~stop ~mode:(`TCP (`Port port)) config
   | Some (crt, key, Some password) ->
     Server.create
       ~stop
       ~mode:(`OpenSSL
                (`Crt_file_path crt,
                 `Key_file_path key,
                 `Password password,
                 `Port port))
       config
   | Some (crt, key, None) ->
     Server.create
       ~stop
       ~mode:(`OpenSSL
                (`Crt_file_path crt,
                 `Key_file_path key,
                 `No_password,
                 `Port port))
       config)
  >>= fun () ->
  Lwt.return (Lwt.wakeup stop_wakener ())
