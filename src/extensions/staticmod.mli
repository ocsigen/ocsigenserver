val section : Lwt_log_core.section (** use Lwt_log.Section.set_level in order to debug *)

val dir : string Ocsigen_server.Site.Config.key
val regexp : string Ocsigen_server.Site.Config.key
val code : string Ocsigen_server.Site.Config.key
val dest :
  Ocsigen_extensions.ud_string Ocsigen_server.Site.Config.key
val root_checks :
  Ocsigen_extensions.ud_string Ocsigen_server.Site.Config.key

val extension : Ocsigen_server.Site.extension
