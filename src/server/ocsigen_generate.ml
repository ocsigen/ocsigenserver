open Lwt
open Cohttp

open Ocsigen_lib
open Ocsigen_http_frame
open Ocsigen_headers
open Ocsigen_http_com
open Ocsigen_socket
open Ocsigen_cookies
open Ocsigen_request_info

exception Ocsigen_upload_forbidden

type to_write =
    No_File of string * Buffer.t
  | A_File of (string * string * string * Unix.file_descr
               * ((string * string) * (string * string) list) option)

let get_boundary ctparams = List.assoc "boundary" ctparams
let counter = let c = ref (Random.int 1000000) in fun () -> c := !c + 1 ; !c

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
  Lwt.catch
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
      | Ocsigen_stream.String_too_large -> Lwt.fail Input_is_too_large
      | e -> Lwt.fail e)

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
      Lwt.return ()
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
      Lwt.return
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
      Lwt.return ()
  in
  Multipart.scan_multipart_body_from_stream
    body bound create add stop maxuploadfilesize >>= fun () ->
  (*VVV Does scan_multipart_body_from_stream read until the end or
    only what it needs?  If we do not consume here, the following
    request will be read only when this one is finished ...  *)
  Ocsigen_stream.consume body_gen >>= fun () ->
  Lwt.return (!params, !files)

let of_cohttp_request ~address ~port ?(receiver=Ocsigen_http_com.dummy_receiver ())
    filenames sockaddr request body =

  let client_inet_addr = ip_of_sockaddr sockaddr in
  let ipstring = Unix.string_of_inet_addr client_inet_addr in

  let meth =
    Ocsigen_http_frame.Http_header.meth_of_cohttp_meth @@
    Request.meth request
  in
  let clientproto =
    Ocsigen_http_frame.Http_header.proto_of_cohttp_version @@
    Request.version request
  in
  let url = Uri.to_string @@ Request.uri request in
  let http_frame =
    Ocsigen_http_frame.of_cohttp_request request body
  in
  let (_, headerhost, headerport, url, path, params, get_params) =
    Url.parse url in
  let headerhost, headerport =
    match headerhost with
    | None -> get_host_from_host_header http_frame
    | _ -> headerhost, headerport
  in
  if clientproto = Ocsigen_http_frame.Http_header.HTTP11 && headerhost = None
  then raise Ocsigen_Bad_Request;

  let useragent = get_user_agent http_frame in
  let cookies_string = lazy (get_cookie_string http_frame) in
  let cookies = lazy (match (Lazy.force cookies_string) with
      | None -> CookiesTable.empty
      | Some s -> parse_cookies s) in
  let ifmodifiedsince = get_if_modified_since http_frame in
  let ifunmodifiedsince = get_if_unmodified_since http_frame in
  let ifnonematch = get_if_none_match http_frame in
  let ifmatch = get_if_match http_frame in

  let ct_string = get_content_type http_frame in
  let ct = Ocsigen_headers.parse_content_type ct_string in
  let cl = get_content_length http_frame in

  let referer = lazy (get_referer http_frame) in
  let origin = lazy (get_origin http_frame) in

  let access_control_request_method =
    lazy (get_access_control_request_method http_frame) in
  let access_control_request_headers =
    lazy (get_access_control_request_headers http_frame) in

  let accept = lazy (get_accept http_frame) in
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
          Some (fun ci -> match !r with
              | None -> let res = f ci in r := Some res; res
              | Some r -> r)
      end
    | _ -> failwith "of_cohttp_request: HTTP method not implemented"
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

  let path_string = Url.string_of_url_path ~encode:true path in

  Lwt.return
    (Ocsigen_request_info.make
       ~url_string:url
       ~meth:meth
       ~protocol:http_frame.Ocsigen_http_frame.frame_header.Ocsigen_http_frame.Http_header.proto
       ~full_path_string:path_string
       ~full_path:path
       ~get_params_string:params
       ~host:headerhost
       ~port_from_host_field:headerport
       ~get_params:get_params
       ~post_params:post_params
       ~files:files
       ~remote_inet_addr:client_inet_addr
       ~remote_ip:ipstring
       ~remote_port:(port_of_sockaddr sockaddr)
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
       ~http_frame:http_frame (* XXX: not tested ! *)
       ~client:receiver (* XXX: it's obsolete with Cohttp ! *)
       ~range:(lazy (Ocsigen_range.get_range http_frame))
       ()
    )

let to_cohttp_request ri =
  let ri_url_string = Ocsigen_request_info.url_string ri in
  let ri_http_frame = Ocsigen_request_info.http_frame ri in
  let ri_protocol = Ocsigen_request_info.protocol ri in
  let uri = Uri.of_string ri_url_string in
  let (request, body) =
    Ocsigen_http_frame.to_cohttp_request ri_http_frame uri in
  let version =
    Ocsigen_http_frame.Http_header.proto_to_cohttp_version ri_protocol in
  let meth = Cohttp.Request.meth request in
  let headers = Cohttp.Request.headers request in
  (meth, version, headers, uri, body)

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

let of_ocsigen_request
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
         (Ocsigen_request_info.make
            ~url_string:url
            ~meth:meth
            ~protocol:http_frame.Ocsigen_http_frame.frame_header.Ocsigen_http_frame.Http_header.proto
            ~ssl:(Lwt_ssl.is_ssl (Ocsigen_http_com.connection_fd receiver))
            ~full_path_string:path_string
            ~full_path:path
            ~get_params_string:params
            ~host:headerhost
            ~port_from_host_field:headerport
            ~get_params:get_params
            ~post_params:post_params
            ~files:files
            ~remote_inet_addr:client_inet_addr
            ~remote_ip:ipstring
            ~remote_port:(port_of_sockaddr sockaddr)
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
            ~client:receiver
            ~range:(lazy (Ocsigen_range.get_range http_frame))
            ()
         )
    )
    (fun e ->
       Ocsigen_messages.debug (fun () -> "~~~ Exn during get_request_infos : "^
                                         Printexc.to_string e);
       Lwt.fail e)
