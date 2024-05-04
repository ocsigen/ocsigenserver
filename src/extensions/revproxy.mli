val section : Lwt_log_core.section
(** use Lwt_log.Section.set_level in order to debug *)

type redirection

val create_redirection :
   ?full_url:[< `Maybe | `No | `Yes > `Yes]
  -> ?pipeline:bool
  -> ?keephost:bool
  -> regexp:string
  -> string
  -> redirection

val run : ?site:Ocsigen_server.Site.t -> redirection:redirection -> unit -> unit
