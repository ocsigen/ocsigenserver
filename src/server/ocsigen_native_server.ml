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

module RI = Ocsigen_request_info (* An alias convenient for accessor *)

exception Ocsigen_Is_a_directory of (Ocsigen_request_info.request_info -> Neturl.url)
exception Ocsigen_unsupported_media
exception Ssl_Exception
exception Socket_closed

(* XXX: This is not the right place ! *)
let shutdown = ref false

let stop m n =
  errlog m; exit n

let ssl_context = Ocsigen_http_client.sslcontext

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

let number_of_client,
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

let http_url_syntax = Hashtbl.find Neturl.common_url_syntax "http"
let counter = let c = ref (Random.int 1000000) in fun () -> c := !c + 1 ; !c
let try_bind' f g h = Lwt.try_bind f h g

(* reading the request *)
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
          if meth ri = Http_header.GET ||
             meth ri = Http_header.HEAD then
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
    match handle_header if_match @@ RI.ifmatch ri with
    | `Precondition_failed -> `Precondition_failed
    | `No_header | `Ignore_header ->
      match handle_header if_unmodified_since @@ RI.ifunmodifiedsince ri with
      | `Precondition_failed -> `Precondition_failed
      | `No_header | `Ignore_header ->
        match handle_header if_none_match @@ RI.ifnonematch ri with
        | `Precondition_failed -> `Precondition_failed
        | `Ignore_header_and_ModifiedSince -> `Std
        | `Unmodified | `No_header as r1 ->
          (match handle_header if_modified_since @@ RI.ifmodifiedsince ri with
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
             Ocsigen_generate.of_ocsigen_request
               meth clientproto url request filenames sockaddr
               port receiver sender_slot)
          (fun ri ->
             (* *** Now we generate the page and send it *)
             (* Log *)
             accesslog
               (try
                  let x_forwarded_for = Http_headers.find Http_headers.x_forwarded_for
                      (RI.http_frame ri).frame_header.Http_header.headers in
                  Format.sprintf
                    "connection for %s from %s (%s) with X-Forwarded-For: %s: %s"
                    (match RI.host ri with
                     | None   -> "<host not specified in the request>"
                     | Some h -> h)
                    (RI.remote_ip ri)
                    (RI.user_agent ri)
                    x_forwarded_for
                    (RI.url_string ri)
                with
                | Not_found ->
                  Format.sprintf
                    "connection for %s from %s (%s): %s"
                    (match RI.host ri with
                     | None   -> "<host not specified in the request>"
                     | Some h -> h)
                    (RI.remote_ip ri)
                    (RI.user_agent ri)
                    (RI.url_string ri));
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

(*

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

*)

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
       (if number_of_client () < max
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
                  Lwt_ssl.ssl_accept s !ssl_context
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
  Lwt_list.iter_p (fun x ->
      wait_end_init >>= fun () ->
      wait_connection use_ssl port x extensions_connector)
    listening_sockets

let service ~address ~port ~connector () =
  let wait_end_init = Lwt.return () in
  listen false (Ocsigen_socket.socket_type_of_string address, port) wait_end_init connector
