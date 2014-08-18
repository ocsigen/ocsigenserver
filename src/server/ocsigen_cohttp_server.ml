open Lwt
open Ocsigen_messages
open Ocsigen_socket
open Ocsigen_lib
open Ocsigen_request_info
open Ocsigen_http_frame
open Ocsigen_headers
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
exception Ocsigen_http_error of (Ocsigen_cookies.cookieset * int)

module Connection = struct
  exception Lost_connection of exn
  exception Aborted
  exception Timeout
  exception Keepalive_timeout
  exception Connection_closed
end

let section = Lwt_log.Section.make "server"

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

let waiters = Hashtbl.create 256

let handler ~address ~port ~extensions_connector (edn, conn) request body =
  let filenames = ref [] in
  let sockaddr = Lwt_unix_conduit.sockname edn in

  let (waiter, wakener) = Lwt.wait () in
  Hashtbl.add waiters conn wakener;

  let handle_error exn =
    let string_of_exn = Printexc.to_string exn in

    match exn with
    | Ocsigen_http_error (cookies_to_set, code) ->
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
            body
            waiter)
        (fun ri ->
           let log =
             Ocsigen_log.of_string "%h %l %t \"%r\""
             |> fun str res -> Ocsigen_log.to_string ri res str
           in
           Lwt.try_bind
             (extensions_connector ri)
             (fun result ->
                Ocsigen_range.compute_range ri result
                >|= To_cohttp.to_response_and_body
                >>= fun (res, body) ->
                Lwt.return (Ocsigen_messages.warning (log res))
                >>= fun () -> Lwt.return (res, body))
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

let conn_closed conn () =
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
  let conn_closed _ () = () in
  let callback = handler ~address ~port ~extensions_connector:connector in
  let config = { Server.callback; Server.conn_closed; } in
  (match ssl with
   | None -> Server.create ~stop ~mode:(`TCP (`Port port)) config
   | Some (crt, key, Some password) ->
     Server.create
       ~stop
       ~mode:(`SSL
                (`Crt_file_path crt,
                 `Key_file_path key,
                 `Password password,
                 `Port port))
       config
   | Some (crt, key, None) ->
     Server.create
       ~stop
       ~mode:(`SSL
                (`Crt_file_path crt,
                 `Key_file_path key,
                 `No_password,
                 `Port port))
       config)
  >>= fun () ->
  Lwt.return (Lwt.wakeup stop_wakener ())
