type t

val make :
  ?body : Cohttp_lwt_body.t ->
  ?cookies : Ocsigen_cookies.cookieset ->
  Cohttp.Response.t ->
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

val set_status : t -> Cohttp.Code.status -> t

val cookies : t -> Ocsigen_cookies.cookieset

val add_cookies : t -> Ocsigen_cookies.cookieset -> t

val header : t -> Ocsigen_header.Name.t -> string option

val header_multi : t -> Ocsigen_header.Name.t -> string list

val add_header : t -> Ocsigen_header.Name.t -> string -> t

val add_header_multi : t -> Ocsigen_header.Name.t -> string list -> t

val replace_header : t -> Ocsigen_header.Name.t -> string -> t

val replace_headers : t -> (Ocsigen_header.Name.t * string) list -> t

val remove_header : t -> Ocsigen_header.Name.t -> t
