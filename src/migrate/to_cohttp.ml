module Cookie = struct

  open Ocsigen_cookies
  open Ocsigen_lib

  let serialize_cookie_raw path exp name c secure =
    Format.sprintf "%s=%s; path=/%s%s%s"
      name c (Url.string_of_url_path ~encode:true path)
      (if secure then "; secure" else "")
      (match exp with
       | Some s -> "; expires=" ^
                   Netdate.format
                     "%a, %d-%b-%Y %H:%M:%S GMT"
                     (Netdate.create s)
       | None -> "")

  let serialize_cookies path table headers =
    CookiesTable.fold
      (fun name c h ->
         let exp, v, secure = match c with
           | Ocsigen_cookies.OUnset -> (Some 0., "", false)
           | Ocsigen_cookies.OSet (t, v, secure) -> (t, v, secure)
         in
         Http_headers.add
           Http_headers.set_cookie (serialize_cookie_raw path exp name v secure)
           h)
      table
      headers

  let serialize cookies headers =
    Cookies.fold serialize_cookies cookies headers
end

let to_version vrs =
  let open Ocsigen_http_frame.Http_header in
  match vrs with
  | HTTP10 -> `HTTP_1_0
  | HTTP11 -> `HTTP_1_1

let to_meth meth =
  let open Ocsigen_http_frame.Http_header in
  match meth with
  | GET -> `GET
  | POST -> `POST
  | HEAD -> `HEAD
  | PUT -> `PUT
  | DELETE -> `DELETE
  | OPTIONS -> `OPTIONS
  | PATCH -> `PATCH
  | UNLINK -> `Other "UNLINK"
  | LINK -> `Other "LINK"
  | TRACE -> `Other "TRACE"
  | CONNECT -> `Other "CONNECT"

let to_headers : Http_headers.t -> Cohttp.Header.t =
  fun x -> x

let to_response ?encoding ?flush
    {
      Ocsigen_http_frame.Http_header.mode;
      Ocsigen_http_frame.Http_header.proto;
      Ocsigen_http_frame.Http_header.headers;
    } =
  let open Ocsigen_http_frame.Http_header in
  match mode with
  | Answer code ->
    let version = to_version proto in
    let status = Cohttp.Code.status_of_code code in
    let headers = to_headers headers in
    Cohttp.Response.make ~version ~status ?flush ?encoding ~headers ()
  | _ -> raise
           (Invalid_argument "Ocsigen_http_frame.Http_header.to_cohttp_response")

let to_request ?encoding
    {
      Ocsigen_http_frame.Http_header.mode;
      Ocsigen_http_frame.Http_header.proto;
      Ocsigen_http_frame.Http_header.headers;
    } uri =
  let open Ocsigen_http_frame.Http_header in
  match mode with
  | Query (meth, _) ->
    let meth = to_meth meth in
    let version = to_version proto in
    let headers = to_headers headers in
    Cohttp.Request.make ~meth ~version ?encoding ~headers uri
  | _ -> raise
           (Invalid_argument "Ocsigen_http_frame.Http_header.to_cohttp_request")

let to_request_and_body ?encoding
    {
      Ocsigen_http_frame.frame_header;
      Ocsigen_http_frame.frame_content;
    } uri =
  let stream = match frame_content with
    | Some s -> Ocsigen_stream.to_lwt_stream
                  ~is_empty:(fun x -> String.length x = 0)
                  s
    | None -> (Lwt_stream.from (fun () -> Lwt.return None) : string Lwt_stream.t)
  in
  (to_request ?encoding frame_header uri, Cohttp_lwt_body.of_stream stream)

let to_date date =
  let x = Netdate.mk_mail_date ~zone:0 date |> Bytes.unsafe_of_string in
  try
    let ind_plus = Bytes.index x '+' in
    Bytes.set x ind_plus 'G';
    Bytes.set x (ind_plus + 1) 'M';
    Bytes.set x (ind_plus + 2) 'T';
    Bytes.sub x 0 (ind_plus + 3)
  with Invalid_argument _ | Not_found -> (); x

let to_type ty charset =
  if String.length ty >= 4 then
    match String.sub ty 0 4, charset with
    | "text", Some "" -> ty
    | "text", Some ch -> Format.sprintf "%s; charset=%s" ty ch
    | _ ->
      begin match String.sub ty (String.length ty - 4) 4, charset with
        | ("+xml"|"/xml"), Some "" -> ty
        | ("+xml"|"/xml"), Some ch -> Format.sprintf "%s; charset=%s" ty ch
        | _ -> ty
      end
  else ty

let to_response_and_body res =
  let res_code = Ocsigen_http_frame.Result.code res in
  let res_etag = Ocsigen_http_frame.Result.etag res in
  let res_cookies = Ocsigen_http_frame.Result.cookies res in
  let res_stream = Ocsigen_http_frame.Result.stream res in
  let res_lastmodified = Ocsigen_http_frame.Result.lastmodified res in
  let res_content_length =  Ocsigen_http_frame.Result.content_length res in
  let res_content_type = Ocsigen_http_frame.Result.content_type res in
  let res_headers = Ocsigen_http_frame.Result.headers res in
  let res_charset = Ocsigen_http_frame.Result.charset res in
  let res_location = Ocsigen_http_frame.Result.location res in
  let headers =
    to_headers (Cookie.serialize res_cookies res_headers) in
  let headers = match res_lastmodified with
    | Some date -> Cohttp.Header.add headers "Last-Modified" (to_date date)
    | None -> headers
  in
  let headers = match res_etag with
    | Some etag -> Cohttp.Header.add headers "ETag" (Format.sprintf "\"%s\"" etag)
    | None -> headers
  in
  let encoding = match res_content_length with
    | Some length when length <= Int64.of_int max_int ->
      Cohttp.Transfer.Fixed length
    | _ -> Cohttp.Transfer.Chunked
  in
  let headers = match res_content_type with
    | Some ty -> Cohttp.Header.add headers "Content-Type" (to_type ty res_charset)
    | None -> headers
  in
  let headers = match res_location with
    | Some location -> Cohttp.Header.add headers "Location" location
    | None -> headers
  in
  (Cohttp.Response.make
     ~status:(Cohttp.Code.status_of_code res_code)
     ~encoding
     ~headers
     (),
   Cohttp_lwt_body.of_stream (Ocsigen_stream.to_lwt_stream
                                ~is_empty:(fun x -> String.length x = 0)
                                (fst res_stream)))
