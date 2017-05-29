val mode :
  [ `Rewrite of (Ocsigen_header.Name.t * Pcre.regexp * string)
  | `Add of (Ocsigen_header.Name.t * string * bool option)
  | `Code of Cohttp.Code.status
  ] Ocsigen_server.Vhost.Config.key

val register : Ocsigen_server.Vhost.t -> unit
