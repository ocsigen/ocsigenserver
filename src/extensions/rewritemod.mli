val section : Lwt_log_core.section
(** use Lwt_log.Section.set_level in order to debug *)

val run :
   ?continue:bool
  -> ?full_rewrite:bool
  -> regexp:string
  -> string
  -> unit
  -> Ocsigen_server.instruction
