val section : Lwt_log_core.section
(** use Lwt_log.Section.set_level in order to debug *)

(** Use the following config key to set options 
    for a statically linked executable without configuration file: *)

val run :
   ?dir:string
  -> ?regexp:string
  -> ?dest:string
  -> ?code:string
  -> ?cache:int
  -> ?root:string
  -> unit
  -> Ocsigen_server.Site.instruction
