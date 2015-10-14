(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2005
 * Vincent Balat, Denis Berthod, Nataliya Guts, Jérôme Vouillon
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*)

open Lwt
open Ocsigen_messages
open Ocsigen_socket
open Ocsigen_lib
open Ocsigen_extensions
open Ocsigen_http_frame
open Ocsigen_headers
open Ocsigen_http_com
open Ocsigen_senders
open Ocsigen_config
open Ocsigen_parseconfig
open Ocsigen_cookies
open Lazy

exception Ocsigen_unsupported_media
exception Ssl_Exception
exception Ocsigen_upload_forbidden
exception Socket_closed

let shutdown = ref false

let () = Random.self_init ()

let () = Ocsigen_commandline.cmdline
(* This is only to have the module Ocsigen_commandline linked
   when we do not use -linkall *)

(* Without the following line, it stops with "Broken Pipe" without raising
   an exception ... *)
let _ = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let section = Lwt_log.Section.make "ocsigen:main"

(* Initialize exception handler for Lwt timeouts: *)
let _ =
  Lwt_timeout.set_exn_handler
    (fun e -> Lwt_log.ign_error ~section ~exn:e "Uncaught Exception after lwt \
                                                 timeout")

let sslctx = Ocsigen_http_client.sslcontext


let ip_of_sockaddr = function
  | Unix.ADDR_INET (ip, port) -> ip
  | _ -> raise (Ocsigen_Internal_Error "ip of unix socket")

let port_of_sockaddr = function
  | Unix.ADDR_INET (ip, port) -> port
  | _ -> raise (Ocsigen_Internal_Error "port of unix socket")


let get_boundary ctparams = List.assoc "boundary" ctparams

let find_field field content_disp =
  let (_, res) = Netstring_pcre.search_forward
      (Netstring_pcre.regexp (field^"=.([^\"]*).;?")) content_disp 0 in
  Netstring_pcre.matched_group res 1 content_disp

type to_write =
    No_File of string * Buffer.t
  | A_File of (string * string * string * Unix.file_descr
               * ((string * string) * (string * string) list) option)

let counter = let c = ref (Random.int 1000000) in fun () -> c := !c + 1 ; !c

let warn sockaddr s =
  Lwt_log.ign_warning_f ~section "While talking to %a:%s"
    (fun () sockaddr ->
       Unix.string_of_inet_addr (ip_of_sockaddr sockaddr)) sockaddr s
let dbg sockaddr s =
  Lwt_log.ign_info_f ~section "While talking to %a:%s"
    (fun () sockaddr ->
       Unix.string_of_inet_addr (ip_of_sockaddr sockaddr)) sockaddr s



let http_url_syntax = Hashtbl.find Neturl.common_url_syntax "http"

let rec find_post_params http_frame ct filenames =
  match http_frame.Ocsigen_http_frame.frame_content with
  | None -> None
  | Some body_gen ->
    let ((ct, cst), ctparams) = match ct with
      (* RFC 2616, sect. 7.2.1 *)
      (* If the media type remains unknown, the recipient SHOULD
         treat it as type "application/octet-stream". *)
      | None -> (("application", "octet-stream"), [])
      | Some (c, p) -> (c, p)
    in
    match String.lowercase ct, String.lowercase cst with
    | "application", "x-www-form-urlencoded" ->
      Some (find_post_params_form_urlencoded body_gen)
    | "multipart", "form-data" ->
      Some (find_post_params_multipart_form_data
              body_gen ctparams filenames)
    | _ -> None

and find_post_params_form_urlencoded body_gen _ =
  catch
    (fun () ->
       let body = Ocsigen_stream.get body_gen in
       (* BY, adapted from a previous comment. Should this stream be
          consumed in case of error? *)
       Ocsigen_stream.string_of_stream
         (Ocsigen_config.get_maxrequestbodysizeinmemory ())
         body >>= fun r ->
       let r = Url.fixup_url_string r in
       Lwt.return ((Netencoding.Url.dest_url_encoded_parameters r), [])
    )
    (function
      | Ocsigen_stream.String_too_large -> fail Input_is_too_large
      | e -> fail e)

and find_post_params_multipart_form_data body_gen ctparams filenames
    (uploaddir, maxuploadfilesize) =
  (* Same question here, should this stream be consumed after an error ? *)
  let body = Ocsigen_stream.get body_gen
  and bound = get_boundary ctparams
  and params = ref []
  and files = ref [] in
  let create hs =
    let content_type =
      try
        let ct = List.assoc "content-type" hs in
        Ocsigen_headers.parse_content_type (Some ct)
      with _ -> None
    in
    let cd = List.assoc "content-disposition" hs in
    let p_name = find_field "name" cd in
    try
      let store = find_field "filename" cd in
      match uploaddir with
      | Some dname ->
        let now = Printf.sprintf "%f-%d"
            (Unix.gettimeofday ()) (counter ()) in
        let fname = dname^"/"^now in
        let fd = Unix.openfile fname
            [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY; Unix.O_NONBLOCK] 0o666
        in
        Lwt_log.ign_info_f ~section "Upload file opened: %s" fname;
        filenames := fname::!filenames;
        A_File (p_name, fname, store, fd, content_type)
      | None -> raise Ocsigen_upload_forbidden
    with Not_found -> No_File (p_name, Buffer.create 1024)
  in
  let rec add where s =
    match where with
    | No_File (p_name, to_buf) ->
      Buffer.add_string to_buf s;
      return ()
    | A_File (_,_,_,wh,_) ->
      let len = String.length s in
      let r = Unix.write wh s 0 len in
      if r < len then
        (*XXXX Inefficient if s is long *)
        add where (String.sub s r (len - r))
      else
        Lwt_unix.yield ()
  in
  let stop size = function
    | No_File (p_name, to_buf) ->
      return
        (params := !params @ [(p_name, Buffer.contents to_buf)])
    (* a la fin ? *)
    | A_File (p_name,fname,oname,wh, content_type) ->
      files :=
        !files@[(p_name, {tmp_filename=fname;
                          filesize=size;
                          raw_original_filename=oname;
                          original_basename=(Filename.basename oname);
                          file_content_type = content_type;
                         })];
      Unix.close wh;
      return ()
  in
  Multipart.scan_multipart_body_from_stream
    body bound create add stop maxuploadfilesize >>= fun () ->
  (*VVV Does scan_multipart_body_from_stream read until the end or
    only what it needs?  If we do not consume here, the following
    request will be read only when this one is finished ...  *)
  Ocsigen_stream.consume body_gen >>= fun () ->
  Lwt.return (!params, !files)

let wrap_stream f x frame_content =
  Ocsigen_stream.make ~finalize:(fun outcome ->
      match frame_content with
      | Some stream ->
        Ocsigen_stream.finalize stream outcome
      | None ->
        Lwt.return ()
    )
    (fun () ->
       f x >>= fun () ->
       match frame_content with
       | Some stream ->
         Ocsigen_stream.next (Ocsigen_stream.get stream)
       | None ->
         Ocsigen_stream.empty None
    )

let handle_100_continue slot frame =
  { frame with
    frame_content = Some (wrap_stream send_100_continue slot
                            frame.frame_content)
  }

let handle_expect slot frame =
  let expect_list = Ocsigen_headers.get_expect frame in
  let proto = Http_header.get_proto frame.frame_header in
  List.fold_left (fun frame tok ->
      match String.lowercase tok with
      | "100-continue" ->
        if proto = Http_header.HTTP11 then
          handle_100_continue slot frame
        else
          frame
      | _ ->
        raise (Ocsigen_http_error (Ocsigen_cookies.empty_cookieset, 417))
    ) frame expect_list

(* reading the request *)
let get_request_infos
    meth clientproto url http_frame filenames sockaddr port receiver
    sender_slot =

  Lwt.catch
    (fun () ->

       let (_, headerhost, headerport, url, path, params, get_params) =
         Url.parse url
       in

       let headerhost, headerport =
         match headerhost with
         | None -> get_host_from_host_header http_frame
         | _ -> headerhost, headerport
       in

       (* RFC:
          1. If Request-URI is an absoluteURI, the host is part of the Request-URI.
          Any Host header field value in the request MUST be ignored.
          2. If the Request-URI is not an absoluteURI, and the request includes a
          Host header field, the host is determined by the Host header field value.
          3. If the host as determined by rule 1 or 2 is not a valid host on the
           server, the response MUST be a 400 (Bad Request) error message.
       *)
       (*  Here we don't trust the port information given by the request.
           We use the port we are listening on. *)
       Lwt_log.ign_info_f ~section "host=%s"
         (match headerhost with None -> "<none>" | Some h -> h);

       (* Servers MUST report a 400 (Bad Request) error if an HTTP/1.1
          request does not include a Host request-header. *)

       if clientproto = Ocsigen_http_frame.Http_header.HTTP11
       && headerhost = None
       then raise Ocsigen_Bad_Request;

       let useragent = get_user_agent http_frame in

       let cookies_string = lazy (get_cookie_string http_frame) in

       let cookies =
         lazy (match (Lazy.force cookies_string) with
             | None -> CookiesTable.empty
             | Some s -> parse_cookies s)
       in

       let ifmodifiedsince = get_if_modified_since http_frame in

       let ifunmodifiedsince =  get_if_unmodified_since http_frame in

       let ifnonematch = get_if_none_match http_frame in

       let ifmatch = get_if_match http_frame in

       let client_inet_addr = ip_of_sockaddr sockaddr in

       let ct_string = get_content_type http_frame in

       let ct = Ocsigen_headers.parse_content_type ct_string in

       let cl = get_content_length http_frame in

       let referer = lazy (get_referer http_frame) in

       let origin = lazy (get_origin http_frame) in

       let access_control_request_method =
         lazy (get_access_control_request_method http_frame) in

       let access_control_request_headers =
         lazy (get_access_control_request_headers http_frame) in

       let accept = lazy (get_accept http_frame)   in

       let accept_charset = lazy (get_accept_charset http_frame) in

       let accept_encoding = lazy (get_accept_encoding http_frame) in

       let accept_language = lazy (get_accept_language http_frame) in

       let post_params0 =
         match meth with
         | Http_header.GET
         | Http_header.DELETE
         | Http_header.PUT
         | Http_header.HEAD -> None
         | Http_header.POST
         | Http_header.OPTIONS ->
           begin
             match find_post_params http_frame ct filenames with
             | None -> None
             | Some f ->
               let r = ref None in
               Some (fun ci ->
                   match !r with
                   | None -> let res = f ci in
                     r := Some res;
                     res
                   | Some r -> r)
           end
         | _ -> failwith "get_request_infos: HTTP method not implemented"
       in
       let post_params =
         match post_params0 with
         | None -> None
         | Some f -> Some (fun ci -> f ci >>= fun (a, _) -> Lwt.return a)
       in
       let files =
         match post_params0 with
         | None -> None
         | Some f -> Some (fun ci -> f ci >>= fun (_, b) -> Lwt.return b)
       in

       let ipstring = Unix.string_of_inet_addr client_inet_addr in
       let path_string = Url.string_of_url_path ~encode:true path in

       Lwt.return
         (Ocsigen_request_info.make
            ~url_string:url
            ~meth:meth
            ~protocol:http_frame.Ocsigen_http_frame
                      .frame_header.Ocsigen_http_frame.Http_header
                      .proto
            ~ssl:(Lwt_ssl.is_ssl (Ocsigen_http_com.connection_fd receiver))
            ~full_path_string:path_string
            ~full_path:path
            ~original_full_path_string:path_string
            ~original_full_path:path
            ~sub_path:path
            ~sub_path_string:(Url.string_of_url_path ~encode:true path)
            ~get_params_string:params
            ~host:headerhost
            ~port_from_host_field:headerport
            ~get_params:get_params
            ~initial_get_params:get_params
            ~post_params:post_params
            ~files:files
            ~remote_inet_addr:client_inet_addr
            ~remote_ip:ipstring
            ~remote_ip_parsed:(lazy (Ipaddr.of_string_exn ipstring))
            ~remote_port:(port_of_sockaddr sockaddr)
            ~forward_ip:[]
            ~server_port:port
            ~user_agent:useragent
            ~cookies_string:cookies_string
            ~cookies:cookies
            ~ifmodifiedsince:ifmodifiedsince
            ~ifunmodifiedsince:ifunmodifiedsince
            ~ifnonematch:ifnonematch
            ~ifmatch:ifmatch
            ~content_type:ct
            ~content_type_string:ct_string
            ~content_length:cl
            ~referer:referer
            ~origin:origin
            ~access_control_request_method:access_control_request_method
            ~access_control_request_headers:access_control_request_headers
            ~accept:accept
            ~accept_charset:accept_charset
            ~accept_encoding:accept_encoding
            ~accept_language:accept_language
            ~http_frame:(handle_expect sender_slot http_frame)
            ~request_cache:(Polytables.create () )
            ~client:(Ocsigen_extensions.client_of_connection receiver)
            ~range:(lazy (Ocsigen_range.get_range http_frame))
            ~timeofday:(Unix.gettimeofday ())
            ~nb_tries:0
            ~connection_closed:(Ocsigen_http_com.closed receiver) ())
    )
    (fun e ->
       Lwt_log.ign_info ~section ~exn:e "Exn during get_request_infos";
       Lwt.fail e)


(* An http result [res] frame has been computed. Depending on
   the If-(None-)?Match and If-(Un)?Modified-Since headers of [ri],
   we return this frame, a 304: Not-Modified, or a 412: Precondition Failed.
   See RFC 2616, sections 14.24, 14.25, 14.26, 14.28 and 13.3.4
*)
let handle_result_frame ri res send =
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
    send (Result.update (Ocsigen_http_frame.Result.empty ())
            ~code:304  (* Not modified *)
            ~lastmodified:(Result.lastmodified res)
            ~etag:(Result.etag res)
            ~headers ())

  | `Precondition_failed ->
    Lwt_log.ign_info ~section
      "Sending 412 Precondition Failed (conditional headers)";
    Ocsigen_stream.finalize (fst (Result.stream res)) `Success >>= fun () ->
    send (Result.update (Ocsigen_http_frame.Result.empty ())
            ~code:412 (* Precondition failed *) ())

  | `Std ->
    Ocsigen_range.compute_range ri res
    >>= send

let service receiver sender_slot request meth url port sockaddr =
  (* sender_slot is here for pipelining:
     we must wait before sending the page,
     because the previous one may not be sent *)

  let head = meth = Http_header.HEAD in
  let clientproto =
    Http_header.get_proto request.Ocsigen_http_frame.frame_header in

  let handle_service_errors e =
    (* Exceptions during page generation *)
    Lwt_log.ign_info ~section ~exn:e "Exception during generation/sending";
    let send_error ?cookies code =
      Ocsigen_senders.send_error ~exn:e sender_slot ~clientproto ?cookies ~head
        ~code ~sender:Ocsigen_http_com.default_sender ()
    in
    match e with
    (* EXCEPTIONS WHILE COMPUTING A PAGE *)
    | Ocsigen_http_error (cookies_to_set, i) ->
      Lwt_log.ign_info_f ~section
        "Sending HTTP error %d %s"
        i
        (Ocsigen_http_frame.Http_error.expl_of_code i);
      send_error ~cookies:cookies_to_set i
    | Ocsigen_stream.Interrupted Ocsigen_stream.Already_read ->
      Lwt_log.ign_warning ~section
        "Cannot read the request twice. You probably have \
         two incompatible options in <site> configuration, \
         or the order of the options in the config file is wrong.";
      send_error 500 (* Internal error *)
    | Unix.Unix_error (Unix.EACCES,_,_)
    | Ocsigen_upload_forbidden ->
      Lwt_log.ign_info ~section "Sending 403 Forbidden";
      send_error 403
    | Http_error.Http_exception (code,_,_) ->
      Ocsigen_http_frame.Http_error.display_http_exception e;
      send_error code
    | Ocsigen_Bad_Request ->
      Lwt_log.ign_info ~section "Sending 400";
      send_error 400
    | Ocsigen_unsupported_media ->
      Lwt_log.ign_info ~section "Sending 415";
      send_error 415
    | Neturl.Malformed_URL ->
      Lwt_log.ign_info ~section "Sending 400 (Malformed URL)";
      send_error 400
    | Ocsigen_Request_too_long ->
      Lwt_log.ign_info ~section "Sending 413 (Entity too large)";
      send_error 413
    | e ->
      Lwt_log.ign_warning_f ~section ~exn:e
        "Exn during page generation (sending 500)";
      send_error 500
  in
  let finish_request () =
    (* We asynchronously finish to read the request contents if this
       is not done yet so that:
       - we can handle the next request
       - there is no dead-lock with the client writing the request and
         the server writing the response.
       We need to do this once the request has been handled before sending
       any reply to the client. *)
    match request.Ocsigen_http_frame.frame_content with
    | Some f ->
      ignore
        (Lwt.catch
           (fun () ->
              Ocsigen_stream.finalize f `Success
              (* will consume the stream and unlock the mutex
                 if not already done *)
           )
           (function
             | e ->

               (match e with
                | Ocsigen_http_com.Lost_connection _ ->
                  warn sockaddr "connection abruptly closed by peer \
                                 while reading contents"
                | Ocsigen_http_com.Timeout ->
                  warn sockaddr "timeout while reading contents"
                | Ocsigen_http_com.Aborted ->
                  dbg sockaddr "reading thread aborted"
                | Http_error.Http_exception (code, mesg, _) ->
                  warn sockaddr (Http_error.string_of_http_exception e)
                | _ ->
                  Ocsigen_messages.unexpected_exception
                    e "Server.finish_request"
               );
               Ocsigen_http_com.abort receiver;
               (* We unlock the receiver in order to resume the
                  reading loop.  As the connection has been aborted,
                  the next read will fail and the connection will be
                  closed properly. *)
               Ocsigen_http_com.unlock_receiver receiver;
               Lwt.return ()))
    | None ->
      ()
  in

  (* body of service *)
  if meth <> Http_header.GET &&
     meth <> Http_header.POST &&
     meth <> Http_header.HEAD &&
     meth <> Http_header.OPTIONS &&
     meth <> Http_header.DELETE &&
     meth <> Http_header.PUT
  then begin
    (* VVV Warning: This must be done once and only once.
       Put this somewhere else to ensure that?
    *)
    warn sockaddr ("Bad request: \""^url^"\"");
    Ocsigen_http_com.wakeup_next_request receiver;
    finish_request ();
    (* RFC 2616, sect 5.1.1 *)
    send_error
      sender_slot ~clientproto ~head ~code:501
      ~sender:Ocsigen_http_com.default_sender ()
  end else begin
    let filenames = ref [] (* All the files sent by the request *) in

    Lwt.finalize (fun () ->
        (* *** First of all, we read the whole the request
           (that will possibly create files) *)
        Lwt.try_bind
          (fun () ->
             get_request_infos
               meth clientproto url request filenames sockaddr
               port receiver sender_slot)
          (fun ri ->
             (* *** Now we generate the page and send it *)
             (* Log *)
             Ocsigen_messages.accesslog
               (try
                  let x_forwarded_for = Http_headers.find
                      Http_headers.x_forwarded_for
                      (Ocsigen_request_info.http_frame ri)
                      .frame_header.Http_header.headers in
                  Format.sprintf
                    "connection for %s from %s (%s) with X-Forwarded-For: \
                     %s: %s"
                    (match Ocsigen_request_info.host ri with
                     | None   -> "<host not specified in the request>"
                     | Some h -> h)
                    (Ocsigen_request_info.remote_ip ri)
                    (Ocsigen_request_info.user_agent ri)
                    x_forwarded_for
                    (Ocsigen_request_info.url_string ri)
                with
                | Not_found ->
                  Format.sprintf
                    "connection for %s from %s (%s): %s"
                    (match Ocsigen_request_info.host ri with
                     | None   -> "<host not specified in the request>"
                     | Some h -> h)
                    (Ocsigen_request_info.remote_ip ri)
                    (Ocsigen_request_info.user_agent ri)
                    (Ocsigen_request_info.url_string ri));
             let send_aux =
               send sender_slot ~clientproto ~head
                 ~sender:Ocsigen_http_com.default_sender
             in

             (* Generation of pages is delegated to extensions: *)
             Lwt.try_bind
               (fun () -> Ocsigen_extensions.compute_result
                   ~awake_next_request:true ri)
               (fun res ->
                  finish_request ();
                  handle_result_frame ri res send_aux
               )
               (fun e ->
                  finish_request ();
                  match e with
                  | Ocsigen_extensions.Ocsigen_Is_a_directory fun_request ->
                    (* User requested a directory. We redirect it to
                       the correct url (with a slash), so that relative
                       urls become correct *)
                    let new_url = fun_request ri in
                    send_aux
                      (Result.update (Ocsigen_http_frame.Result.empty ())
                         ~code:301
                         ~location:(Some (Neturl.string_of_url new_url)) ())
                  | _ -> handle_service_errors e
               )
          )
          (fun e ->
             warn sockaddr ("Bad request: \""^url^"\"");
             Ocsigen_http_com.wakeup_next_request receiver;
             finish_request ();
             handle_service_errors e
          ))
      (fun () ->
         (* We remove all the files created by the request
            (files sent by the client) *)
         if !filenames <> [] then Lwt_log.ign_info ~section "** Removing files";
         List.iter
           (fun a ->
              try Unix.unlink a
              with Unix.Unix_error _ as e ->
                Lwt_log.ign_warning_f ~section ~exn:e "Error while removing \
                                                       file %s" a )
           !filenames;
         return ())
  end

let linger in_ch receiver =
  Lwt.catch
    (fun () ->
       (* We wait for 30 seconds at most and close the connection
          after 2 seconds without receiving data from the client *)
       let abort_fun () = Lwt_ssl.abort in_ch Exit in
       let long_timeout = Lwt_timeout.create 30 abort_fun in
       let short_timeout = Lwt_timeout.create 2 abort_fun in
       Lwt_timeout.start long_timeout;
       let s = String.create 1024 in

       let rec linger_aux () =
         Lwt_ssl.wait_read in_ch >>= fun () ->
         Lwt.try_bind
           (fun () ->
              Lwt_timeout.start short_timeout;
              Lwt_ssl.read in_ch s 0 1024)
           (fun len ->
              if len > 0 then linger_aux () else Lwt.return ())
           (fun e ->
              begin match e with
                  Unix.Unix_error(Unix.ECONNRESET,_,_)
                | Ssl.Read_error (Ssl.Error_syscall | Ssl.Error_ssl)
                | Exit ->
                  Lwt.return ()
                | _ ->
                  Lwt.fail e
              end)
       in
       (* We start the lingering reads before waiting for the
          senders to terminate in order to avoid a deadlock *)
       let linger_thread = linger_aux () in
       Ocsigen_http_com.wait_all_senders receiver >>= fun () ->
       Lwt_log.ign_info ~section "** SHUTDOWN";
       Lwt_ssl.ssl_shutdown in_ch >>= fun () ->
       Lwt_ssl.shutdown in_ch Unix.SHUTDOWN_SEND;
       linger_thread >>= fun () ->
       Lwt_timeout.stop long_timeout;
       Lwt_timeout.stop short_timeout;
       Lwt.return ())
    (fun e ->
       Ocsigen_messages.unexpected_exception e "Server.linger"; Lwt.return ())

let try_bind' f g h = Lwt.try_bind f h g

let add_to_receivers_waiting_for_pipeline,
    remove_from_receivers_waiting_for_pipeline,
    iter_receivers_waiting_for_pipeline =
  let l = Clist.create () in
  ((fun r ->
      let node = Clist.make r in
      Clist.insert l node;
      node),
   Clist.remove,
   (fun f ->
      Clist.fold_left
        (fun t v ->
           (*VVV reread this. Is yield here ok? *)
           t >>= Lwt_unix.yield >>= fun () ->
           f v)
        (Lwt.return ())
        l))

let handle_connection port in_ch sockaddr =
  let receiver = Ocsigen_http_com.create_receiver
      (Ocsigen_config.get_client_timeout ()) Query in_ch
  in

  let handle_write_errors e =
    begin match e with
      | Lost_connection e' ->
        warn sockaddr ("connection abruptly closed by peer ("
                       ^ Printexc.to_string e' ^ ")")
      | Ocsigen_http_com.Timeout ->
        warn sockaddr "timeout"
      | Ocsigen_http_com.Aborted ->
        dbg sockaddr "writing thread aborted"
      | Ocsigen_stream.Interrupted e' ->
        warn sockaddr ("interrupted content stream ("
                       ^ Printexc.to_string e' ^ ")")
      | _ ->
        Ocsigen_messages.unexpected_exception e "Server.handle_write_errors"
    end;
    Ocsigen_http_com.abort receiver;
    Lwt.fail Ocsigen_http_com.Aborted
  in

  let handle_read_errors e =
    begin match e with
      | Ocsigen_http_com.Connection_closed ->
        (* This is the clean way to terminate the connection *)
        dbg sockaddr "connection closed by peer";
        Ocsigen_http_com.abort receiver;
        Ocsigen_http_com.wait_all_senders receiver
      | Ocsigen_http_com.Keepalive_timeout ->
        dbg sockaddr "keepalive timeout";
        Ocsigen_http_com.abort receiver;
        Ocsigen_http_com.wait_all_senders receiver
      | Ocsigen_http_com.Lost_connection _ ->
        warn sockaddr "connection abruptly closed by peer";
        Ocsigen_http_com.abort receiver;
        Ocsigen_http_com.wait_all_senders receiver
      | Ocsigen_http_com.Timeout ->
        warn sockaddr "timeout";
        Ocsigen_http_com.abort receiver;
        Ocsigen_http_com.wait_all_senders receiver
      | Ocsigen_http_com.Aborted ->
        dbg sockaddr "reading thread aborted";
        Ocsigen_http_com.wait_all_senders receiver
      | Http_error.Http_exception (code, mes, _) ->
        warn sockaddr (Http_error.string_of_http_exception e);
        Ocsigen_http_com.start_processing receiver (fun slot ->
            (*XXX We should use the right information for clientproto
              and head... *)
            send_error slot
              ~clientproto:Ocsigen_http_frame.Http_header.HTTP10
              ~head:false
              (* ~keep_alive:false *)
              ~exn:e
              ~sender:Ocsigen_http_com.default_sender ());
        linger in_ch receiver
      | _ ->
        Ocsigen_messages.unexpected_exception e "Server.handle_read_errors";
        Ocsigen_http_com.abort receiver;
        Ocsigen_http_com.wait_all_senders receiver
    end
  in

  let rec handle_request ?receiver_pos () =
    try_bind'
      (fun () ->
         Lwt_log.ign_info ~section "** Receiving HTTP message";
         (if Ocsigen_config.get_respect_pipeline () then
            (* if we lock this mutex, requests from a same connection will be sent
               to extensions in the same order they are received on pipeline.
               It is locked only in server. Ocsigen_http_client has its own mutex.
               (*VVV use the same? *)
            *)
            Ocsigen_http_com.block_next_request receiver
          else
            Lwt.return ())
         >>= fun () ->
         Ocsigen_http_com.get_http_frame receiver)
      (fun exn ->
         (* We remove the receiver from the set of requests
            waiting for pipeline *)
         (match receiver_pos with
          | Some pos -> remove_from_receivers_waiting_for_pipeline pos
          | None -> ());
         handle_read_errors exn)
      (fun request ->
         (* As above *)
         (match receiver_pos with
          | Some pos -> remove_from_receivers_waiting_for_pipeline pos
          | None -> ());
         let meth, url =
           match
             Http_header.get_firstline request.Ocsigen_http_frame.frame_header
           with
           | Http_header.Query a -> a
           | _                   -> assert false
           (*XXX Should be checked in [get_http_frame] *)
         in
         Ocsigen_http_com.start_processing receiver (fun slot ->
             Lwt.catch
               (fun () ->
                  (*XXX Why do we need the port but not the host name? *)
                  service receiver slot request meth url port sockaddr)
               handle_write_errors);
         if not !shutdown &&
            get_keepalive request.Ocsigen_http_frame.frame_header
         then
           (* We put the receiver in the set of receiver waiting for
              pipeline in order to be able to shutdown the connections
              if the server is shutting down.
           *)
           handle_request
             ~receiver_pos:(add_to_receivers_waiting_for_pipeline receiver) ()
         else (* No keep-alive => no pipeline *)
           (* We wait for the query to be entirely read and for
              the reply to be sent *)
           Ocsigen_http_com.lock_receiver receiver >>= fun () ->
           Ocsigen_http_com.wait_all_senders receiver >>= fun () ->
           Lwt_ssl.ssl_shutdown in_ch
      )

  in (* body of handle_connection *)
  handle_request ()

let rec wait_connection use_ssl port socket =
  let handle_exn e =
    Lwt_unix.yield () >>= fun () -> match e with
    | Socket_closed ->
      Lwt_log.ign_info ~section "Socket closed";
      Lwt.return ()
    | Unix.Unix_error ((Unix.EMFILE | Unix.ENFILE), _, _) ->
      (* this should not happen, report it *)
      Lwt_log.ign_error ~section
        "Max number of file descriptors reached unexpectedly, please check...";
      wait_connection use_ssl port socket
    | e ->
      Lwt_log.ign_info_f ~section ~exn:e "Accept failed";
      wait_connection use_ssl port socket
  in
  try_bind'
    (fun () ->
       (* if too much connections,
          we wait for a signal before accepting again *)
       let max = get_max_number_of_connections () in
       (if get_number_of_connected () < max
        then Lwt.return ()
        else begin
          Lwt_log.ign_warning_f ~section
            "Max simultaneous connections (%d) reached."
            (get_max_number_of_connections ());
          wait_fewer_connected max
        end) >>= fun () ->
       (* We do several accept(), as explained in
          "Accept()able strategies ..." by Tim Brecht & al. *)
       Lwt_unix.accept_n socket 50)
    handle_exn
    (fun (l, e) ->
       let number_of_accepts = List.length l in
       Lwt_log.ign_info_f ~section "received %d accepts" number_of_accepts;
       incr_connected number_of_accepts;
       if e = None then ignore (wait_connection use_ssl port socket);

       let handle_one (s, sockaddr) =
         Lwt_log.ign_info ~section
           "** New CONNECTION";
         Lwt.catch
           (fun () ->
              Lwt_unix.set_close_on_exec s;
              Lwt_unix.setsockopt s Unix.TCP_NODELAY true;
              begin if use_ssl then
                  Lwt_ssl.ssl_accept s !sslctx
                else
                  Lwt.return (Lwt_ssl.plain s)
              end >>= fun in_ch ->
              handle_connection port in_ch sockaddr)
           (fun e ->
              Ocsigen_messages.unexpected_exception e
                "Server.wait_connection (handle connection)";
              (match e with
              | Ssl.Accept_error(Ssl.Error_ssl|Ssl.Error_syscall) ->
                Ocsigen_messages.warning
                  ("Last SSL error: " ^ Ssl.get_error_string ())
              | _ -> ());
              return ())
         >>= fun () ->
         Lwt_log.ign_info ~section "** CLOSE";
         catch
           (fun () -> Lwt_unix.close s)
           (function Unix.Unix_error _ as e ->
             Ocsigen_messages.unexpected_exception
               e "Server.wait_connection (close)";
             Lwt.return ()
                   | e -> Lwt.fail e)
         >>= decr_connected
       in

       Lwt_list.iter_p handle_one l >>= fun () ->
       match e with
       | Some e -> handle_exn e
       | None -> Lwt.return ())


(* fatal errors messages *)
let errmsg = function
  | Dynlink_wrapper.Error e ->
    (("Fatal - Dynamic linking error: "^(Dynlink_wrapper.error_message e)),
     6)
  | (Unix.Unix_error _) as e ->
    (("Fatal - "^(Printexc.to_string e)),
     9)
  | Ssl.Private_key_error ->
    (("Fatal - bad password"),
     10)
  | Ocsigen_config.Config_file_error msg
  | Ocsigen_extensions.Error_in_config_file msg ->
    (("Fatal - Error in configuration file: "^msg),
     50)
  | Simplexmlparser.Xml_parser_error s ->
    (("Fatal - Error in configuration file: "^s),
     51)
  | Ocsigen_loader.Dynlink_error (s, exn) ->
    (("Fatal - While loading "^s^": "^(Printexc.to_string exn)),
     52)
  | Ocsigen_loader.Findlib_error _ as e ->
    (("Fatal - " ^ Printexc.to_string e), 53)
  | exn ->
    try
      ((Ocsigen_extensions.get_init_exn_handler () exn),
       20)
    with
      exn ->
      (("Fatal - Uncaught exception: "^Printexc.to_string exn),
       100)




(* loading new configuration *)
let reload_conf s =
  try
    Ocsigen_extensions.start_initialisation ();

    parse_server true s;

    Ocsigen_extensions.end_initialisation ();
  with e ->
    Ocsigen_extensions.end_initialisation ();
    Lwt_log.ign_error ~section (fst (errmsg e))

(* reloading the config file *)
let reload ?file () =

  (* That function cannot be interrupted??? *)
  Lwt_log.ign_warning ~section "Reloading config file" ;

  (try
     match parse_config ?file () with
     | [] -> ()
     | s::_ -> reload_conf s
   with e -> Lwt_log.ign_error ~section (fst (errmsg e)));

  Lwt_log.ign_warning ~section "Config file reloaded"


let shutdown_server s l =
  try
    let timeout = match l with
      | [] -> Ocsigen_config.get_shutdown_timeout ()
      | ["notimeout"] -> None
      | [t] ->
        Some (float_of_string t)
      | _ -> failwith "syntax error in command"
    in
    Lwt_log.ign_warning ~section "Shutting down";
    List.iter
      (fun s -> Lwt_unix.abort s Socket_closed) !sockets;
    List.iter
      (fun s -> Lwt_unix.abort s Socket_closed) !sslsockets;
    sockets := [];
    sslsockets := [];
    shutdown := true;
    if Ocsigen_extensions.get_number_of_connected () <= 0
    then exit 0;
    (match timeout with
     | Some t -> ignore (Lwt_unix.sleep t >>= fun () -> exit 0)
     | None -> ());
    ignore
      (iter_receivers_waiting_for_pipeline
         (fun receiver ->
            (*VVV reread this - why are we using infinite iterators? *)
            Ocsigen_http_com.wait_all_senders receiver >>= fun () ->
            Ocsigen_http_com.abort receiver;
            Lwt.return ()));
  with Failure e ->
    Lwt_log.ign_warning_f ~section "Wrong command: %s (%s)" s e


let _ =
  let f s = function
    | ["reopen_logs"] ->
      Ocsigen_messages.open_files () >>= fun () ->
      Lwt_log.ign_warning ~section "Log files reopened";
      Lwt.return ()
    | ["reload"] -> reload (); Lwt.return ()
    | ["reload"; file] -> reload ~file (); Lwt.return ()
    | "shutdown"::l -> shutdown_server s l; Lwt.return ()
    | ["gc"] ->
      Gc.compact ();
      Lwt_log.ign_warning ~section "Heap compaction requested by user";
      Lwt.return ()
    | ["clearcache"] -> Ocsigen_cache.clear_all_caches ();
      Lwt.return ()
    | _ -> Lwt.fail Ocsigen_command.Unknown_command
  in
  Ocsigen_command.register_command_function f

exception Stop of int * string

let start_server () =
  let stop n fmt = Printf.ksprintf (fun s -> raise (Stop (n, s))) fmt in
  (** Thread waiting for events on a the listening port *)
  let listen use_ssl (addr, port) wait_end_init =
    let listening_sockets =
      try
        let sockets = make_sockets addr port in
        List.iter (fun x -> Lwt_unix.listen x 1024) sockets;
        sockets
      with
      | Unix.Unix_error (Unix.EACCES, _, _) ->
        stop 7 "Fatal - You are not allowed to use port %d." port
      | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
        stop 8 "Fatal - The port %d is already in use." port
      | exn ->
        stop 100 "Fatal - Uncaught exception: %s" (Printexc.to_string exn)
    in
    List.iter (fun x ->
      ignore (wait_end_init >>= fun () ->
              wait_connection use_ssl port x)) listening_sockets;
    listening_sockets
  in
  try

    (* initialization functions for modules (Ocsigen extensions or application
       code) loaded from now on will be executed directly. *)
    Ocsigen_loader.set_init_on_load true;

    let config_servers = parse_config () in

    let number_of_servers = List.length config_servers in

    if number_of_servers > 1
    then Lwt_log.ign_warning ~section "Multiple servers not supported anymore";

    let ask_for_passwd sslports _ =
      print_string "Please enter the password for the HTTPS server listening \
                    on port(s) ";
      print_string (String.concat ", " (List.map (fun (_,p) -> string_of_int p)
                                          sslports));
      print_string ": ";
      let old_term= Unix.tcgetattr Unix.stdin in
      let old_echo = old_term.Unix.c_echo in
      old_term.Unix.c_echo <- false;
      Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH old_term;
      try
        let r = read_line () in
        print_newline ();
        old_term.Unix.c_echo <- old_echo;
        Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH old_term;
        r
      with exn ->
        old_term.Unix.c_echo <- old_echo;
        Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH old_term;
        raise exn
    in
    let run (user, group) (_, ports, sslports) (minthreads, maxthreads) s =

      Ocsigen_messages.open_files ~user ~group () >>= fun () ->

      let wait_end_init, wait_end_init_awakener = wait () in
      (* Listening on all ports: *)
      sockets := List.fold_left
          (fun a i -> (listen false i wait_end_init) @ a) [] ports;
      sslsockets := List.fold_left
          (fun a i -> (listen true i wait_end_init) @ a) [] sslports;

      begin match ports with
        | (_, p)::_ -> Ocsigen_config.set_default_port p
        | _ -> ()
      end;
      begin match sslports with
        | (_, p)::_ -> Ocsigen_config.set_default_sslport p
        | _ -> ()
      end;

      let current_uid = Unix.getuid () in

      let gid = match group with
        | None -> Unix.getgid ()
        | Some group -> (try
                           (Unix.getgrnam group).Unix.gr_gid
                         with Not_found as e ->
                           Lwt_log.ign_error ~section "Error: Wrong group";
                           raise e)
      in

      let uid = match user with
        | None -> current_uid
        | Some user -> (try
                          (Unix.getpwnam user).Unix.pw_uid
                        with Not_found as e ->
                          Lwt_log.ign_error ~section "Error: Wrong user";
                          raise e)
      in

      (* A pipe to communicate with the server *)
      let commandpipe = get_command_pipe () in
      (try
         ignore (Unix.stat commandpipe);
       with Unix.Unix_error _ ->
         (try
            let umask = Unix.umask 0 in
            Unix.mkfifo commandpipe 0o660;
            Unix.chown commandpipe uid gid;
            ignore (Unix.umask umask);
            Lwt_log.ign_warning ~section "Command pipe created";
          with e ->
            Lwt_log.ign_error ~section ~exn:e
              "Cannot create the command pipe"));

      (* I change the user for the process *)
      begin try
          if current_uid = 0 then begin
            match user with
            | None -> ()
            | Some user -> Unix.initgroups user gid
          end;
          Unix.setgid gid;
          Unix.setuid uid;
        with (Unix.Unix_error _ | Failure _) as e ->
          Lwt_log.ign_error ~section "Error: Wrong user or group";
          raise e
      end;

      Ocsigen_config.set_user user;
      Ocsigen_config.set_group group;

      (* Je suis fou :
         let rec f () =
           print_endline "-";
           Lwt_unix.yield () >>= f
           in f (); *)

      if maxthreads < minthreads
      then
        raise
          (Config_file_error "maxthreads should be greater than minthreads");

      ignore (Ocsigen_config.init_preempt
                minthreads
                maxthreads
                (fun s -> Lwt_log.ign_error ~section s));

      (* Now I can load the modules *)
      Dynlink_wrapper.init ();
      Dynlink_wrapper.allow_unsafe_modules true;

      Ocsigen_extensions.start_initialisation ();

      parse_server false s;

      Dynlink_wrapper.prohibit ["Ocsigen_extensions.R"];
      (* As libraries are reloaded each time the config file is read,
         we do not allow to register extensions in libraries *)
      (* seems it does not work :-/ *)
      (* Closing stderr, stdout stdin if silent *)

      if (Ocsigen_config.get_silent ())
      then begin
        (* redirect stdout and stderr to /dev/null *)
        let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 in
        Unix.dup2 devnull Unix.stdout;
        Unix.dup2 devnull Unix.stderr;
        Unix.close devnull;
        Unix.close Unix.stdin;
      end;

      (* detach from the terminal *)
      if (Ocsigen_config.get_daemon ())
      then ignore (Unix.setsid ());

      Ocsigen_extensions.end_initialisation ();

      let pipe = Lwt_chan.in_channel_of_descr
          (Lwt_unix.of_unix_file_descr
             (Unix.openfile commandpipe
                [Unix.O_RDWR; Unix.O_NONBLOCK; Unix.O_APPEND] 0o660)) in

      let rec f () =
        Lwt_chan.input_line pipe >>= fun s ->
        Ocsigen_messages.warning ("Command received: "^s);
        (Lwt.catch
           (fun () ->
              let prefix, c =
                match String.split ~multisep:true ' ' s with
                | [] -> raise Ocsigen_command.Unknown_command
                | a::l ->
                  try
                    let aa, ab = String.sep ':' a in
                    (Some aa, (ab::l))
                  with Not_found -> None, (a::l)
              in
              Ocsigen_command.get_command_function () ?prefix s c)
           (function
             | Ocsigen_command.Unknown_command ->
               Lwt_log.ign_warning ~section "Unknown command";
               Lwt.return ()
             | e ->
               Lwt_log.ign_error ~section ~exn:e "Uncaught Exception after \
                                                  command";
               Lwt.fail e))
        >>= f
      in ignore (f ());

      Lwt.async_exception_hook := (fun e ->
        (* replace the default "exit 2" behaviour *)
        Lwt_log.ign_error ~section ~exn:e "Uncaught Exception"
      );

      Lwt.wakeup wait_end_init_awakener ();

      Lwt_log.ign_warning ~section "Ocsigen has been launched \
                                    (initialisations ok)";

      fst (Lwt.wait ())

    in

    let set_passwd_if_needed (ssl, ports, sslports) =
      if sslports <> []
      then
        match ssl with
        | None
        | Some (None, None) -> ()
        | Some (None, _) -> raise (Ocsigen_config.Config_file_error
                                     "SSL certificate is missing")
        | Some (_, None) -> raise (Ocsigen_config.Config_file_error
                                     "SSL key is missing")
        | Some ((Some c), (Some k)) ->
          Ssl.set_password_callback !sslctx (ask_for_passwd sslports);
          Ssl.use_certificate !sslctx c k
    in

    let write_pid pid =
      match Ocsigen_config.get_pidfile () with
        None -> ()
      | Some p ->
        let spid = (string_of_int pid)^"\n" in
        let len = String.length spid in
        let f =
          Unix.openfile
            p
            [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o640 in
        ignore (Unix.write f spid 0 len);
        Unix.close f
    in

    let rec launch = function
      | [] -> ()
      | [h] ->
        let user_info, sslinfo, threadinfo = extract_info h in
        set_passwd_if_needed sslinfo;
        if (get_daemon ())
        then
          let pid = Lwt_unix.fork () in
          if pid = 0
          then
            Lwt_main.run (run user_info sslinfo threadinfo h)
          else begin
            Ocsigen_messages.console
              (fun () -> "Process "^(string_of_int pid)^" detached");
            write_pid pid
          end
        else begin
          write_pid (Unix.getpid ());
          Lwt_main.run (run user_info sslinfo threadinfo h)
        end
      | _ -> () (* Multiple servers not supported any more *)

    in
    launch config_servers

  with
  | Stop (n, s) ->
    Lwt_main.run (Lwt_log.error ~section s);
    exit n
  | e ->
    let msg, errno = errmsg e in
    Lwt_main.run (Lwt_log.error ~section msg);
    exit errno
