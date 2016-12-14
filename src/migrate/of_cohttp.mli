(** Module to cast Cohttp value to OcsigenServer value *)

(** [of_version] cast version of protocol *)
val of_version : Cohttp.Code.version -> Ocsigen_http_frame.Http_header.proto

(** [of_meth] cast method of request *)
val of_meth : Cohttp.Code.meth -> Ocsigen_http_frame.Http_header.http_method

(** [of_request] extracts only header of Cohttp request *)
val of_request :
  Cohttp.Request.t -> Ocsigen_http_frame.Http_header.http_header

(** [of_response] extracts only header of Cohttp response *)
val of_response :
  Cohttp.Response.t -> Ocsigen_http_frame.Http_header.http_header

(** [of_request_and_body] cast Cohttp request to OcsigenServer request *)
val of_request_and_body :
  Cohttp.Request.t * Cohttp_lwt_body.t -> Ocsigen_http_frame.t

(** [of_date] cast a date (as [string]) to timestamp *)
val of_date : string -> float

val of_charset : string -> string option

(** [of_response_and_body] cast Cohttp response to OcsigenServer response *)
val of_response_and_body :
  Cohttp.Response.t * Cohttp_lwt_body.t -> Ocsigen_http_frame.result

(** [of_response_and_body'] cast Cohttp response to OcsigenServer frame (like a
    request). It's specially used by [revproxy] extension *)
val of_response_and_body' :
  Cohttp.Response.t * Cohttp_lwt_body.t -> Ocsigen_http_frame.t
