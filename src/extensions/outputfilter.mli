val mode :
  [ `Rewrite of (Ocsigen_header.Name.t * Pcre.regexp * string)
  | `Add of (Ocsigen_header.Name.t * string * bool option)
  | `Code of Cohttp.Code.status
  ] Ocsigen_extensions.Virtual_host.Config.key

val register : Ocsigen_extensions.Virtual_host.t -> unit
