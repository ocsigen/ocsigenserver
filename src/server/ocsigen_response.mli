type t = {
  a_response : Cohttp.Response.t ;
  a_body     : Cohttp_lwt_body.t ;
  a_cookies  : Ocsigen_cookies.cookieset
}

val make :
  ?body : Cohttp_lwt_body.t ->
  ?cookies : Ocsigen_cookies.cookieset ->
  response : Cohttp.Response.t ->
  unit ->
  t

val update :
  ?response : Cohttp.Response.t ->
  ?body : Cohttp_lwt_body.t ->
  ?cookies : Ocsigen_cookies.cookieset ->
  t ->
  t

val of_cohttp :
  ?cookies : Ocsigen_cookies.cookieset ->
  (Cohttp.Response.t * Cohttp_lwt_body.t) ->
  t

val to_cohttp : t -> Cohttp.Response.t * Cohttp_lwt_body.t

val status : t -> Cohttp.Code.status

val cookies : t -> Ocsigen_cookies.cookieset

val set_status : t -> Cohttp.Code.status -> t

val add_cookies : t -> Ocsigen_cookies.cookieset -> t

val header : t -> Http_headers.name -> string option

val header_multi : t -> Http_headers.name -> string list

val add_header : t -> Http_headers.name -> string -> t

val add_header_multi : t -> Http_headers.name -> string list -> t

val replace_header : t -> Http_headers.name -> string -> t

val replace_headers : t -> (Http_headers.name * string) list -> t

val remove_header : t -> Http_headers.name -> t
