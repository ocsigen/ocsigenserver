val section : Lwt_log_core.section
(** use Lwt_log.Section.set_level in order to debug *)

type redirection

val create_redirection :
   ?full:[`Maybe | `No | `Yes]
  -> ?temporary:bool
  -> regexp:string
  -> string
  -> redirection

val redirection : redirection Ocsigen_server.Site.Config.key
val extension : Ocsigen_server.Site.extension
