open Lwt
open Ocsigen_messages
open Ocsigen_socket
open Ocsigen_lib
open Ocsigen_request_info
open Ocsigen_http_frame
open Ocsigen_headers
open Ocsigen_http_com
open Ocsigen_senders
open Ocsigen_config
open Ocsigen_cookies
open Ocsigen_generate
open Ocsigen_common_server
open Lazy
open Cohttp
open Cohttp_lwt_unix

module RI = Ocsigen_request_info (* An alias convenient for accessor *)

exception Ocsigen_Is_a_directory of (Ocsigen_request_info.request_info -> Neturl.url)
exception Ocsigen_unsupported_media

(** print_cohttp_request Print request for debug
 * @param out_ch output for debug
 * @param request Cohttp request *)

let print_cohttp_request out_ch request =
  let print_list print_data out_ch lst =
    let rec aux = function
      | [] -> ()
      | [ x ] -> print_data out_ch x
      | x :: r -> print_data out_ch x; aux r
    in aux lst
  in

  let open Cohttp.Request in

  Printf.fprintf out_ch "%s [%s/%s]:\n"
    (Uri.to_string request.uri)
    (Cohttp.Code.string_of_version request.version)
    (Cohttp.Code.string_of_method request.meth);
  Cohttp.Header.iter
    (fun key values ->
       Printf.fprintf out_ch "\t%s = %a\n" key
         (print_list (fun out_ch x -> Printf.fprintf out_ch "%s" x)) values)
    request.headers

let handler ~address ~port ~extensions_connector endpoint request body =
  let filenames = ref [] in
  let sockaddr =
    Unix.ADDR_INET
      (Unix.inet_addr_of_string @@ Server.Connection.addr endpoint,
       Server.Connection.port endpoint) in

  Printf.fprintf stderr "%a%!" print_cohttp_request request;

  let handle_error exn =
    let string_of_exn = Printexc.to_string exn in

    match exn with
    | Ocsigen_http_com.Ocsigen_http_error (cookies_to_set, code) ->
      Server.respond_error 
        ~status:(Cohttp.Code.status_of_code code)
        ~body:string_of_exn
        ()
    | Ocsigen_stream.Interrupted Ocsigen_stream.Already_read ->
      Server.respond_error
        ~status:(Cohttp.Code.status_of_code 500)
        ~body:string_of_exn
        ()
    | Unix.Unix_error (Unix.EACCES, _, _)
    | Ocsigen_upload_forbidden ->
      Server.respond_error
        ~status:(Cohttp.Code.status_of_code 403)
        ~body:string_of_exn
        ()
    | Http_error.Http_exception (code, _, _) ->
      Server.respond_error
        ~status:(Cohttp.Code.status_of_code code)
        ~body:string_of_exn
        ()
    | Ocsigen_Bad_Request ->
      Server.respond_error
        ~status:(Cohttp.Code.status_of_code 400)
        ~body:string_of_exn
        ()
    | Ocsigen_unsupported_media ->
      Server.respond_error
        ~status:(Cohttp.Code.status_of_code 415)
        ~body:string_of_exn
        ()
    | Neturl.Malformed_URL ->
      Server.respond_error
        ~status:(Cohttp.Code.status_of_code 400)
        ~body:string_of_exn
        ()
    | Ocsigen_Request_too_long ->
      Server.respond_error
        ~status:(Cohttp.Code.status_of_code 413)
        ~body:string_of_exn
        ()
    | exn ->
      Server.respond_error
        ~status:(Cohttp.Code.status_of_code 500)
        ~body:string_of_exn
        ()
  in
  Lwt.finalize (fun () ->
      Lwt.try_bind
        (fun () -> Ocsigen_generate.of_cohttp_request
            ~address
            ~port
            filenames
            sockaddr
            request
            body)
        (fun ri ->
           Lwt.try_bind
             (extensions_connector ri)
             (fun res ->
                Ocsigen_range.compute_range ri res >>= fun res ->
                let (res, body) = Ocsigen_http_frame.result_to_cohttp_response res
                in Lwt.return (res, body))
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
           with Unix.Unix_error _ as e ->
             Ocsigen_messages.warning
               (Format.sprintf "Error while removing file %s: %s"
                  a (Printexc.to_string e)))
           !filenames; Lwt.return ())

let stop _ _ = ()

let number_of_client () = 0

let service ?ssl ~address ~port ~connector () =
  let conn_closed _ () = () in
  let callback = handler ~address ~port ~extensions_connector:connector in
  let config = { Server.callback; Server.conn_closed; } in
  match ssl with
   | None -> Server.create ~address ~port config
   | Some (crt, key, password) ->
       Server.create
       ~mode:(`SSL (`Crt_file_path crt, `Key_file_path key, password))
       ~address ~port config
