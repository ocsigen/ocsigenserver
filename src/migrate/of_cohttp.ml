open Ocsigen_lib
open Ocsigen_cookies

let of_version vrs =
  let open Ocsigen_http_frame.Http_header in
  match vrs with
  | `HTTP_1_0 -> HTTP10
  | `HTTP_1_1 -> HTTP11
  | _ -> raise Ocsigen_lib.Ocsigen_Bad_Request

let of_meth meth =
  let open Ocsigen_http_frame.Http_header in
  match meth with
  | `GET -> GET
  | `POST -> POST
  | `HEAD -> HEAD
  | `PUT -> PUT
  | `DELETE -> DELETE
  | `OPTIONS -> OPTIONS
  | `PATCH -> PATCH
  | `TRACE -> TRACE
  | `CONNECT -> CONNECT
  | `Other "LINK" -> LINK
  | `Other "UNLINK" -> UNLINK
  | `Other _ -> raise Ocsigen_lib.Ocsigen_Bad_Request

let of_request req =
  let open Ocsigen_http_frame.Http_header in
  {
    mode = Query
        (of_meth @@ Cohttp.Request.meth req,
         Uri.to_string @@ Cohttp.Request.uri req);
    proto = of_version @@ Cohttp.Request.version req;
    headers =  Cohttp.Request.headers req;
  }

let of_response resp =
  let open Ocsigen_http_frame.Http_header in
  {
    mode = Answer (Cohttp.Code.code_of_status @@ Cohttp.Response.status resp);
    proto = of_version @@ Cohttp.Response.version resp;
    headers =  Cohttp.Response.headers resp;
  }

let of_request_and_body (req, body) =
  let open Ocsigen_http_frame in
  {
    frame_header = of_request req;
    frame_content = Some
        (Ocsigen_stream.of_lwt_stream
           (Cohttp_lwt_body.to_stream body));
    frame_abort = (fun () -> Lwt.return ());
    (* XXX: It's obsolete ! *)
  }

let of_date str =
  (* XXX: handle of GMT ? (see. To_cohttp.to_date) *)
  Netdate.parse_epoch ~localzone:true ~zone:0 str

let of_charset =
  let re = Re_emacs.re ~case:true ".*charset=\\(.*\\)" in
  let ca = Re.(compile (seq ([start; re]))) in
  fun str ->
    try
      let subs = Re.exec ~pos:0 ca str in
      let (start, stop) = Re.get_ofs subs 1 in
      Some (String.sub str start (stop - start))
    with Not_found -> None

let of_response_and_body (resp, body) =
  let cookies = Ocsigen_cookies.Cookies.empty in
  (* VVV: this function is only used by Ocsigen_local_files and this module
   * create an empty Cookie table as response, it's useless to cast header *)
  (* VVV: We could do a conversion function but will do nothing in the end.
   * The conversion function is difficult! *)
  let lastmodified =
    match Cohttp.Header.get (Cohttp.Response.headers resp) "Last-Modified" with
    | None -> None
    | Some date -> Some (of_date date) in
  let etag =
    match Cohttp.Header.get (Cohttp.Response.headers resp) "ETag" with
    | None -> None
    | Some tag -> Scanf.sscanf tag "\"%s\"" (fun x -> Some x) in
  let code = Cohttp.Code.code_of_status @@ Cohttp.Response.status resp in
  let stream =
    (Ocsigen_stream.of_lwt_stream
       (Cohttp_lwt_body.to_stream body), None) in
  (* XXX: I don't want to know what the second value! None! *)
  let content_length =
    let open Cohttp.Transfer in
    match Cohttp.Response.encoding resp with
    | Fixed i -> Some i
    | _  -> None in
  let content_type = Cohttp.Header.get_media_type
    @@ Cohttp.Response.headers resp in
  let headers =  Cohttp.Response.headers resp in
  let charset =
    match Cohttp.Header.get (Cohttp.Response.headers resp) "Content-Type" with
    | None -> None
    | Some ct -> of_charset ct in
  let location =
    Cohttp.Header.get (Cohttp.Response.headers resp) "Location" in
  Ocsigen_http_frame.Result.update (Ocsigen_http_frame.Result.empty ())
    ~cookies
    ~lastmodified
    ~etag
    ~code
    ~stream
    ~content_length
    ~content_type
    ~headers
    ~charset
    ~location ()

(* VVV: Specific casting for revproxy extension *)

let of_response_and_body' (resp, body) =
  let open Ocsigen_http_frame in
  {
    frame_header = of_response resp;
    frame_content = Some
        (Ocsigen_stream.of_lwt_stream
           (Cohttp_lwt_body.to_stream body));
    frame_abort = (fun () -> Lwt.return ());
  }
