(** Staticmod: serve static files *)

(** If you want to use this extension with Ocsigen Server's configuration file, 
   have a look at the <<a_manual chapter="staticmod"|manual page>>.
   If you are using Ocsigen Server as a library, use the interface described
   here.
*)

(**
   This module belongs to ocamlfind package
   [ocsigenserver.ext.staticmod].
*)

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
    Call this if you want to run Ocsigen Server without configuration file.
    The optional parameter correspond to the options of the configuration
    file described <<a_manual chapter="staticmod"|here>>.*)

val section : Lwt_log_core.section
(** Use {!Lwt_log.Section.set_level} in order to select the log level for
   this module *)
