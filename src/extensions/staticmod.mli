val section : Lwt_log_core.section
(** use Lwt_log.Section.set_level in order to debug *)

val run :
   ?dir:string
  -> ?regexp:string
  -> ?dest:string
  -> ?code:string
  -> ?cache:int
  -> ?root:string
  -> unit
  -> Ocsigen_server.instruction
(** Run static mod on a specific directory. 
    Call this if you want to run Ocsigen Server without configuration file. *)
