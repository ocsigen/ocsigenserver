(** Module to cast OcsigenServer value to Cohttp value *)

(** Module to serialize cookie to Cohttp headers *)
module Cookie :
  sig
    val serialize :
      Ocsigen_cookies.cookie Ocsigen_cookies.CookiesTable.t
      Ocsigen_cookies.Cookies.t -> Cohttp.Header.t -> Cohttp.Header.t
  end

(** [to_version] cast version of protocol *)
val to_version :
  Ocsigen_http_frame.Http_header.proto -> [> `HTTP_1_0 | `HTTP_1_1 ]

(** [to_meth] cast method of request *)
val to_meth :
  Ocsigen_http_frame.Http_header.http_method -> Cohttp.Code.meth

(** [to_headers] cast OcsigenServer headers to Cohttp headers (this function is
    [fun x -> x] with simply annotation) *)
val to_headers : Http_headers.t -> Cohttp.Header.t

(** [to_response] injects only header of new Cohttp response *)
val to_response :
  ?encoding:Cohttp.Transfer.encoding ->
  ?flush:bool ->
  Ocsigen_http_frame.Http_header.http_header -> Cohttp.Response.t

(** [to_request] injects only headers to new Cohttp request *)
val to_request :
  ?encoding:Cohttp.Transfer.encoding ->
  Ocsigen_http_frame.Http_header.http_header -> Uri.t -> Cohttp.Request.t

(** [to_request_and_body] cast a OcsigenServer request to Cohttp request *)
val to_request_and_body :
  ?encoding:Cohttp.Transfer.encoding ->
  Ocsigen_http_frame.t ->
  Uri.t -> Cohttp.Request.t * Cohttp_lwt_body.t

(** [to_date] cast a date (as timestamp) to a string *)
val to_date : float -> string

val to_type : string -> string option -> string

(** [to_response_and_body] cast a OcsigenServer response to Cohttp response *)
val to_response_and_body :
  Ocsigen_http_frame.result ->
  Cohttp.Response.t * Cohttp_lwt_body.t
