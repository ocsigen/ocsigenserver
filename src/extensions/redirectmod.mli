val section : Lwt_log_core.section
(** use Lwt_log.Section.set_level in order to debug *)

type redirection

val create_redirection :
   ?full_url:[< `Maybe | `No | `Yes > `Yes]
  -> ?temporary:bool
  -> regexp:string
  -> string
  -> redirection

val run : redirection:redirection -> unit -> Ocsigen_server.Site.instruction
