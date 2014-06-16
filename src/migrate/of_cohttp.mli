val of_version :
  [> `HTTP_1_0 | `HTTP_1_1 ] -> Ocsigen_http_frame.Http_header.proto

val of_meth :
  [< `DELETE
   | `GET
   | `HEAD
   | `OPTIONS
   | `Other of string
   | `PATCH
   | `POST
   | `PUT ] ->
  Ocsigen_http_frame.Http_header.http_method

val of_headers : Cohttp.Header.t -> Http_headers.t

val of_request :
  Cohttp.Request.t -> Ocsigen_http_frame.Http_header.http_header

val of_response :
  Cohttp.Response.t -> Ocsigen_http_frame.Http_header.http_header

val of_request_and_body :
  Cohttp.Request.t * Cohttp_lwt_body.t -> Ocsigen_http_frame.t

val of_date : string -> float

val of_charset : string -> string option

val of_response_and_body :
  Cohttp.Response.t * Cohttp_lwt_body.t -> Ocsigen_http_frame.result

val of_response_and_body' :
  Cohttp.Response.t * Cohttp_lwt_body.t -> Ocsigen_http_frame.t
