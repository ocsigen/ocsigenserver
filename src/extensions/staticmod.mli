val dir : string Ocsigen_server.Vhost.Config.key
val regexp :
  Pcre.regexp Ocsigen_server.Vhost.Config.key
val opt_code :
  Pcre.regexp Ocsigen_server.Vhost.Config.key
val opt_dest :
  Ocsigen_extensions.ud_string Ocsigen_server.Vhost.Config.key
val opt_root_checks :
  Ocsigen_extensions.ud_string Ocsigen_server.Vhost.Config.key

val register : Ocsigen_server.Vhost.t -> unit
