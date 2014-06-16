module Cookie :
  sig
    val serialize :
      Ocsigen_cookies.cookie Ocsigen_cookies.CookiesTable.t
      Ocsigen_cookies.Cookies.t -> Cohttp.Header.t -> Cohttp.Header.t
  end

val to_version :
  Ocsigen_http_frame.Http_header.proto -> [> `HTTP_1_0 | `HTTP_1_1 ]

val to_meth :
  Ocsigen_http_frame.Http_header.http_method ->
  [> `DELETE
   | `GET
   | `HEAD
   | `OPTIONS
   | `Other of string
   | `PATCH
   | `POST
   | `PUT ]

val to_headers : Http_headers.t -> Cohttp.Header.t

val to_response :
  ?encoding:Cohttp.Transfer.encoding ->
  ?flush:bool ->
  Ocsigen_http_frame.Http_header.http_header -> Cohttp.Response.t

val to_request :
  ?encoding:Cohttp.Transfer.encoding ->
  Ocsigen_http_frame.Http_header.http_header -> Uri.t -> Cohttp.Request.t

val to_request_and_body :
  ?encoding:Cohttp.Transfer.encoding ->
  Ocsigen_http_frame.t ->
  Uri.t -> Cohttp.Request.t * [> `Stream of string Lwt_stream.t ]

val to_date : float -> string

val to_type : string -> string option -> string

val to_response_and_body :
  Ocsigen_http_frame.result ->
  Cohttp.Response.t * [> `Stream of string Lwt_stream.t ]
