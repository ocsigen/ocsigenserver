val mode :
  [ `Rewrite of (Ocsigen_header.Name.t * Pcre.regexp * string)
  | `Add of (Ocsigen_header.Name.t * string * bool option)
  | `Code of Cohttp.Code.status
  ] Ocsigen_server.Site.Config.key

val extension : Ocsigen_server.Site.extension
