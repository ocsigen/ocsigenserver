val section : Lwt_log_core.section
(** use Lwt_log.Section.set_level in order to debug *)

val run :
   ?site:Ocsigen_server.Site.t
  -> ?dir:string
  -> ?regexp:string
  -> ?dest:string
  -> ?code:string
  -> ?cache:int
  -> ?root:string
  -> unit
  -> unit
(** Run static mod on a specific directory. 
    Call this if you want to run Ocsigen Server without configuration file. *)
