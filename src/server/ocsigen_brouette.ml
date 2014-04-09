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
open Lazy

exception Ocsigen_Is_a_directory of (Ocsigen_request_info.request_info -> Neturl.url)
exception Ocsigen_unsupported_media
exception Ocsigen_upload_forbidden
exception Ssl_Exception
exception Socket_closed

(* XXX: This is not the right place ! *)
let shutdown = ref false

let stop m n =
  errlog m; exit n

let sslctx = Ocsigen_http_client.sslcontext

let warn sockaddr s =
  let ip = Unix.string_of_inet_addr (ip_of_sockaddr sockaddr) in
  Ocsigen_messages.warning ("While talking to " ^ ip ^ ": " ^ s)

let dbg sockaddr s =
  Ocsigen_messages.debug
    (fun () ->
       let ip = Unix.string_of_inet_addr (ip_of_sockaddr sockaddr) in
       "While talking to " ^ ip ^ ": " ^ s)

(*****************************************************************************)
(* This is used by server.ml.
   I put that here because I need it to be accessible for profiling. *)
let sockets = ref []
let sslsockets = ref []

let get_number_of_connected,
    incr_connected,
    decr_connected,
    wait_fewer_connected =
  let connected = ref 0 in
  let maxr = ref (-1000) in
  let mvar = Lwt_mvar.create_empty () in
  ((fun () -> !connected),
   (fun n -> connected := !connected + n),
   (fun () ->
      let c = !connected in
      connected := c - 1;
      if !connected <= 0 && !sockets = [] && !sslsockets = []
      then exit 0;
      if c = !maxr
      then begin
        Ocsigen_messages.warning "Number of connections now ok";
        maxr := -1000;
        Lwt_mvar.put mvar ()
      end
      else Lwt.return ()
   ),
   (fun max ->
      maxr := max;
      Lwt_mvar.take mvar)
  )

type to_write =
    No_File of string * Buffer.t
  | A_File of (string * string * string * Unix.file_descr
               * ((string * string) * (string * string) list) option)

let get_boundary ctparams = List.assoc "boundary" ctparams
let http_url_syntax = Hashtbl.find Neturl.common_url_syntax "http"
let counter = let c = ref (Random.int 1000000) in fun () -> c := !c + 1 ; !c
let try_bind' f g h = Lwt.try_bind f h g

let find_field field content_disp =
  let (_, res) = Netstring_pcre.search_forward
      (Netstring_pcre.regexp (field^"=.([^\"]*).;?")) content_disp 0 in
  Netstring_pcre.matched_group res 1 content_disp

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
    (uploaddir, maxuploadfilesize)=
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
        Ocsigen_messages.debug2 ("Upload file opened: " ^ fname);
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
      (* Ocsigen_messages.debug "closing file"; *)
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
        raise (Ocsigen_http_com.Ocsigen_http_error (Ocsigen_cookies.empty_cookieset, 417))
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
       Ocsigen_messages.debug
         (fun () ->
            "- host="^(match headerhost with None -> "<none>" | Some h -> h));

       (* Servers MUST report a 400 (Bad Request) error if an HTTP/1.1
          request does not include a Host request-header. *)

       if clientproto = Ocsigen_http_frame.Http_header.HTTP11 && headerhost = None
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
         {ri_url_string = url;
          ri_method = meth;
          ri_protocol = http_frame.Ocsigen_http_frame.frame_header.Ocsigen_http_frame.Http_header.proto;
          ri_ssl = Lwt_ssl.is_ssl (Ocsigen_http_com.connection_fd receiver);
          ri_full_path_string = path_string;
          ri_full_path = path;
          ri_original_full_path_string = path_string;
          ri_original_full_path = path;
          ri_sub_path = path;
          ri_sub_path_string = Url.string_of_url_path ~encode:true path;
          ri_get_params_string = params;
          ri_host = headerhost;
          ri_port_from_host_field = headerport;
          ri_get_params = get_params;
          ri_initial_get_params = get_params;
          ri_post_params = post_params;
          ri_files = files;
          ri_remote_inet_addr = client_inet_addr;
          ri_remote_ip = ipstring;
          ri_remote_ip_parsed = lazy (Ipaddr.of_string_exn ipstring);
          ri_remote_port = port_of_sockaddr sockaddr;
          ri_forward_ip = [];
          ri_server_port = port;
          ri_user_agent = useragent;
          ri_cookies_string = cookies_string;
          ri_cookies = cookies;
          ri_ifmodifiedsince = ifmodifiedsince;
          ri_ifunmodifiedsince = ifunmodifiedsince;
          ri_ifnonematch = ifnonematch;
          ri_ifmatch = ifmatch;
          ri_content_type = ct;
          ri_content_type_string = ct_string;
          ri_content_length = cl;
          ri_referer = referer;
          ri_origin = origin;
          ri_access_control_request_method = access_control_request_method;
          ri_access_control_request_headers = access_control_request_headers;
          ri_accept = accept;
          ri_accept_charset = accept_charset;
          ri_accept_encoding = accept_encoding;
          ri_accept_language = accept_language;
          ri_http_frame = handle_expect sender_slot http_frame;
          ri_request_cache = Polytables.create ();
          ri_client = receiver;
          ri_range = lazy (Ocsigen_range.get_range http_frame);
          ri_timeofday = Unix.gettimeofday ();
          ri_nb_tries = 0;
          ri_connection_closed = Ocsigen_http_com.closed receiver;
         }
    )
    (fun e ->
       Ocsigen_messages.debug (fun () -> "~~~ Exn during get_request_infos : "^
                                         Printexc.to_string e);
       Lwt.fail e)

(* An http result [res] frame has been computed. Depending on
   the If-(None-)?Match and If-(Un)?Modified-Since headers of [ri],
   we return this frame, a 304: Not-Modified, or a 412: Precondition Failed.
   See RFC 2616, sections 14.24, 14.25, 14.26, 14.28 and 13.3.4
*)
let handle_result_frame ri res send =
  (* Subfonctions to handle each header separately *)
  let if_unmodified_since unmodified_since = (* Section 14.28 *)
    if (res.res_code = 412 ||
        (200 <= res.res_code && res.res_code < 300)) then
      match res.res_lastmodified with
      | Some r ->
        if r <= unmodified_since then
          `Ignore_header
        else
          `Precondition_failed
      | None -> `Ignore_header
    else
      `Ignore_header

  and if_modified_since modified_since = (* Section 14.25 *)
    if res.res_code = 200 then
      match res.res_lastmodified with
      | Some r ->
        if r <= modified_since then
          `Unmodified
        else
          `Ignore_header
      | _ -> `Ignore_header
    else
      `Ignore_header

  and if_none_match if_none_match = (* Section 14.26 *)
    if (res.res_code = 412 ||
        (200 <= res.res_code && res.res_code < 300)) then
      match res.res_etag with
      | None   -> `Ignore_header
      | Some e ->
        if List.mem e if_none_match then
          if ri.ri_method = Http_header.GET ||
             ri.ri_method = Http_header.HEAD then
            `Unmodified
          else
            `Precondition_failed
        else
          `Ignore_header_and_ModifiedSince
    else
      `Ignore_header

  and if_match if_match = (* Section 14.24 *)
    if (res.res_code = 412 ||
        (200 <= res.res_code && res.res_code < 300)) then
      match res.res_etag with
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
    match handle_header if_match ri.ri_ifmatch with
    | `Precondition_failed -> `Precondition_failed
    | `No_header | `Ignore_header ->
      match handle_header if_unmodified_since ri.ri_ifunmodifiedsince with
      | `Precondition_failed -> `Precondition_failed
      | `No_header | `Ignore_header ->
        match handle_header if_none_match ri.ri_ifnonematch with
        | `Precondition_failed -> `Precondition_failed
        | `Ignore_header_and_ModifiedSince -> `Std
        | `Unmodified | `No_header as r1 ->
          (match handle_header if_modified_since ri.ri_ifmodifiedsince with
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
    Ocsigen_messages.debug2 "-> Sending 304 Not modified ";
    Ocsigen_stream.finalize (fst res.res_stream) `Success >>= fun () ->
    send { (Ocsigen_http_frame.empty_result ()) with
           res_code = 304  (* Not modified *);
           res_lastmodified = res.res_lastmodified;
           res_etag = res.res_etag;
         }

  | `Precondition_failed ->
    Ocsigen_messages.debug2 "-> Sending 412 Precondition Failed \
                             (conditional headers)";
    Ocsigen_stream.finalize (fst res.res_stream) `Success >>= fun () ->
    send { (Ocsigen_http_frame.empty_result ()) with
           res_code = 412 (* Precondition failed *)}

  | `Std ->
    Ocsigen_range.compute_range ri res
    >>= send



(** service execute a service of client
 * @param receiver information of client
 * @param sender_slot place to write
 * @param request information of request
 * @param meth method of request
 * @param url url of request
 * @param port port of request
 * @param sockaddr information of client socket *)

let service receiver sender_slot request meth url port sockaddr
    extensions_connector =
  (* sender_slot is here for pipelining:
     we must wait before sending the page,
     because the previous one may not be sent *)

  let head = meth = Http_header.HEAD in
  let clientproto =
    Http_header.get_proto request.Ocsigen_http_frame.frame_header in

  let handle_service_errors e =
    (* Exceptions during page generation *)
    Ocsigen_messages.debug
      (fun () -> "~~~ Exception during generation/sending: " ^ Printexc.to_string e);
    let send_error ?cookies code =
      Ocsigen_senders.send_error ~exn:e sender_slot ~clientproto ?cookies ~head
        ~code ~sender:Ocsigen_http_com.default_sender ()
    in
    match e with
    (* EXCEPTIONS WHILE COMPUTING A PAGE *)
    | Ocsigen_http_com.Ocsigen_http_error (cookies_to_set, i) ->
      Ocsigen_messages.debug
        (fun () -> "-> Sending HTTP error "^(string_of_int i)^" "^
                   Ocsigen_http_frame.Http_error.expl_of_code i);
      send_error ~cookies:cookies_to_set i
    | Ocsigen_stream.Interrupted Ocsigen_stream.Already_read ->
      Ocsigen_messages.warning
        "Cannot read the request twice. You probably have \
         two incompatible options in <site> configuration, \
         or the order of the options in the config file is wrong.";
      send_error 500 (* Internal error *)
    | Unix.Unix_error (Unix.EACCES,_,_)
    | Ocsigen_upload_forbidden ->
      Ocsigen_messages.debug2 "-> Sending 403 Forbidden";
      send_error 403
    | Http_error.Http_exception (code,_,_) ->
      Ocsigen_http_frame.Http_error.display_http_exception e;
      send_error code
    | Ocsigen_Bad_Request ->
      Ocsigen_messages.debug2 "-> Sending 400";
      send_error 400
    | Ocsigen_unsupported_media ->
      Ocsigen_messages.debug2 "-> Sending 415";
      send_error 415
    | Neturl.Malformed_URL ->
      Ocsigen_messages.debug2 "-> Sending 400 (Malformed URL)";
      send_error 400
    | Ocsigen_Request_too_long ->
      Ocsigen_messages.debug2 "-> Sending 413 (Entity too large)";
      send_error 413
    | e ->
      Ocsigen_messages.warning
        ("Exn during page generation: " ^ Printexc.to_string e ^" (sending 500)");
      Ocsigen_messages.debug2 "-> Sending 500";
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
             accesslog
               (try
                  let x_forwarded_for = Http_headers.find Http_headers.x_forwarded_for
                      ri.ri_http_frame.frame_header.Http_header.headers in
                  Format.sprintf
                    "connection for %s from %s (%s) with X-Forwarded-For: %s: %s"
                    (match ri.ri_host with
                     | None   -> "<host not specified in the request>"
                     | Some h -> h)
                    ri.ri_remote_ip
                    ri.ri_user_agent
                    x_forwarded_for
                    ri.ri_url_string
                with
                | Not_found ->
                  Format.sprintf
                    "connection for %s from %s (%s): %s"
                    (match ri.ri_host with
                     | None   -> "<host not specified in the request>"
                     | Some h -> h)
                    ri.ri_remote_ip
                    ri.ri_user_agent
                    ri.ri_url_string);
             let send_aux =
               send sender_slot ~clientproto ~head
                 ~sender:Ocsigen_http_com.default_sender
             in

             (* Generation of pages is delegated to extensions: *)
             Lwt.try_bind
               (* (fun () -> Ocsigen_extensions.compute_result
                   ~awake_next_request:true ri) *)
               (extensions_connector ri)
               (fun res ->
                  finish_request ();
                  handle_result_frame ri res send_aux
               )
               (fun e ->
                  finish_request ();
                  match e with
                  | Ocsigen_Is_a_directory fun_request ->
                    (* User requested a directory. We redirect it to
                       the correct url (with a slash), so that relative
                       urls become correct *)
                    (*
                    Ocsigen_messages.debug2 "-> Sending 301 Moved permanently";
                    let port = Ocsigen_extensions.get_port request in
                    let new_url = Neturl.make_url
                        ~scheme:(if ri.ri_ssl then "https" else "http")
                        ~host:(Ocsigen_extensions.get_hostname request)
                        ?port:(if (port = 80 && not ri.ri_ssl)
                               || (ri.ri_ssl && port = 443)
                               then None
                               else Some port)
                        ~path:(""::(Url.add_end_slash_if_missing
                                      ri.ri_full_path))
                        ?query:ri.ri_get_params_string
                        http_url_syntax
                    in
                    *)
                    send_aux {
                      (Ocsigen_http_frame.empty_result ()) with
                      res_code = 301;
                      res_location = Some (Neturl.string_of_url (fun_request ri))
                    }

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
         if !filenames <> [] then Ocsigen_messages.debug2 "** Removing files";
         List.iter
           (fun a ->
              try Unix.unlink a
              with Unix.Unix_error _ as e ->
                Ocsigen_messages.warning
                  (Format.sprintf "Error while removing file %s: %s"
                     a (Printexc.to_string e)))
           !filenames;
         return ())
  end



(** linger wait to close connection
 * @param in_ch channel of client
 * @param receiver information of the client *)

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
       Ocsigen_messages.debug2 "** SHUTDOWN";
       Lwt_ssl.ssl_shutdown in_ch >>= fun () ->
       Lwt_ssl.shutdown in_ch Unix.SHUTDOWN_SEND;
       linger_thread >>= fun () ->
       Lwt_timeout.stop long_timeout;
       Lwt_timeout.stop short_timeout;
       Lwt.return ())
    (fun e ->
       Ocsigen_messages.unexpected_exception e "Server.linger"; Lwt.return ())

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

(** shutdown_server shutdown the server
 * @param s string of command
 * @param l parameter of this commande *)

let shutdown_server s l =
  try
    let timeout = match l with
      | [] -> Ocsigen_config.get_shutdown_timeout ()
      | ["notimeout"] -> None
      | [t] ->
        Some (float_of_string t)
      | _ -> failwith "syntax error in command"
    in
    Ocsigen_messages.warning "Shutting down";
    List.iter
      (fun s -> Lwt_unix.abort s Socket_closed) !sockets;
    List.iter
      (fun s -> Lwt_unix.abort s Socket_closed) !sslsockets;
    sockets := [];
    sslsockets := [];
    shutdown := true;
    if get_number_of_connected () <= 0
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
    Ocsigen_messages.warning ("Wrong command: " ^ s ^ " (" ^ e ^ ")")


(** handle_connection handle a connection to service and respect order of
 * pipeline
 * @param port port of server
 * @param in_ch data of client
 * @param sockaddr socket information of client
 * @extensions_connector connector to extension (eliom) *)

let handle_connection port in_ch sockaddr extensions_connector =
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
        warn sockaddr ("interrupted content stream (" ^ Printexc.to_string e' ^ ")")
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
         Ocsigen_messages.debug2 "** Receiving HTTP message";
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
                  service receiver slot request meth url port sockaddr
                    extensions_connector)
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

(** wait_connection loop of server
 * @param use_ssl use ssl (true) or not
 * @param port port to bind master socket
 * @param socket slave socket (client)
 * @param extensions_connector connector from server to extension (eliom) *)

let rec wait_connection use_ssl port socket extensions_connector =
  let handle_exn e =
    Lwt_unix.yield () >>= fun () -> match e with
    | Socket_closed ->
      Ocsigen_messages.debug2 "Socket closed";
      Lwt.return ()
    | Unix.Unix_error ((Unix.EMFILE | Unix.ENFILE), _, _) ->
      (* this should not happen, report it *)
      Ocsigen_messages.errlog
        "Max number of file descriptors reached unexpectedly, please check...";
      wait_connection use_ssl port socket extensions_connector
    | e ->
      Ocsigen_messages.debug
        (fun () -> Format.sprintf "Accept failed: %s" (Printexc.to_string e));
      wait_connection use_ssl port socket extensions_connector
  in
  try_bind'
    (fun () ->
       (* if too much connections,
          we wait for a signal before accepting again *)
       let max = get_max_number_of_connections () in
       (if get_number_of_connected () < max
        then Lwt.return ()
        else begin
          ignore
            (Ocsigen_messages.warning
               (Format.sprintf "Max simultaneous connections (%d) reached."
                  (get_max_number_of_connections ())));
          wait_fewer_connected max
        end) >>= fun () ->
       (* We do several accept(), as explained in
          "Accept()able strategies ..." by Tim Brecht & al. *)
       Lwt_unix.accept_n socket 50)
    handle_exn
    (fun (l, e) ->
       let number_of_accepts = List.length l in
       Ocsigen_messages.debug
         (fun () -> "received "^string_of_int number_of_accepts^" accepts"  );
       incr_connected number_of_accepts;
       if e = None
       then ignore (wait_connection use_ssl port socket extensions_connector);

       let handle_one (s, sockaddr) =
         Ocsigen_messages.debug2
           "\n__________________NEW CONNECTION__________________________";
         Lwt.catch
           (fun () ->
              Lwt_unix.set_close_on_exec s;
              Lwt_unix.setsockopt s Unix.TCP_NODELAY true;
              begin if use_ssl then
                  Lwt_ssl.ssl_accept s !sslctx
                else
                  Lwt.return (Lwt_ssl.plain s)
              end >>= fun in_ch ->
              handle_connection port in_ch sockaddr extensions_connector)
           (fun e ->
              Ocsigen_messages.unexpected_exception e
                "Server.wait_connection (handle connection)";
              return ())
         >>= fun () ->
         Ocsigen_messages.debug2 "** CLOSE";
         catch
           (fun () -> Lwt_unix.close s)
           (function Unix.Unix_error _ as e ->
             Ocsigen_messages.unexpected_exception
               e "Server.wait_connection (close)";
             Lwt.return ()
                   | e -> Lwt.fail e)
         >>= decr_connected
       in

       Lwt_util.iter handle_one l >>= fun () ->
       match e with
       | Some e -> handle_exn e
       | None -> Lwt.return ())

(** Thread waiting for events on a the listening port
 * @param use_ssl use ssl (true) or not
 * @param addr address to bind server
 * @param port port to bind server
 * @param wait_end_init fix point to wait the initialization *)

let listen use_ssl (addr, port) wait_end_init extensions_connector =
  let listening_sockets =
    try
      let sockets = make_sockets addr port in
      List.iter (fun x -> Lwt_unix.listen x 1024) sockets;
      sockets
    with
    | Unix.Unix_error (Unix.EACCES, _, _) ->
      stop
        (Format.sprintf "Fatal - You are not allowed to use port %d." port)
        7
    | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
      stop (Format.sprintf "Fatal - The port %d is already in use." port) 8
    | exn ->
      stop ("Fatal - Uncaught exception: " ^ Printexc.to_string exn) 100
  in
  List.iter (fun x ->
      ignore (wait_end_init >>= fun () ->
              wait_connection use_ssl port x extensions_connector))
    listening_sockets;
  listening_sockets
