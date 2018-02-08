type t

val make :
  ?body : Cohttp_lwt.Body.t ->
  ?cookies : Ocsigen_cookie_map.t ->
  Cohttp.Response.t ->
  t

val update :
  ?response : Cohttp.Response.t ->
  ?body : Cohttp_lwt.Body.t ->
  ?cookies : Ocsigen_cookie_map.t ->
  t ->
  t

val of_cohttp :
  ?cookies : Ocsigen_cookie_map.t ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) ->
  t

val to_cohttp : t -> Cohttp.Response.t * Cohttp_lwt.Body.t

val status : t -> Cohttp.Code.status

val set_status : t -> Cohttp.Code.status -> t

val cookies : t -> Ocsigen_cookie_map.t

val add_cookies : t -> Ocsigen_cookie_map.t -> t

val header : t -> Ocsigen_header.Name.t -> string option

val header_multi : t -> Ocsigen_header.Name.t -> string list

val add_header : t -> Ocsigen_header.Name.t -> string -> t

val add_header_multi : t -> Ocsigen_header.Name.t -> string list -> t

val replace_header : t -> Ocsigen_header.Name.t -> string -> t

val replace_headers : t -> (Ocsigen_header.Name.t * string) list -> t

val remove_header : t -> Ocsigen_header.Name.t -> t
