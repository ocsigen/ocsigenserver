val dir : string Ocsigen_extensions.Virtual_host.Config.key
val regexp :
  Pcre.regexp Ocsigen_extensions.Virtual_host.Config.key
val opt_code :
  Pcre.regexp Ocsigen_extensions.Virtual_host.Config.key
val opt_dest :
  Ocsigen_extensions.ud_string Ocsigen_extensions.Virtual_host.Config.key
val opt_root_checks :
  Ocsigen_extensions.ud_string Ocsigen_extensions.Virtual_host.Config.key

val register : Ocsigen_extensions.Virtual_host.t -> unit
