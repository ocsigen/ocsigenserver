(** Rewrite: Change the request *)

(** If you want to use this extension with Ocsigen Server's configuration file, 
    have a look at the {% <<a_manual chapter="rewritemod"|manual page>>%}.
    If you are using Ocsigen Server as a library, use the interface described
    here. Each of these functions behaves exactly as its configuration file
    counterpart. 
+*)

(**
This module belongs to ocamlfind package
   [ocsigenserver.ext.rewritemod].
*)

val section : Lwt_log_core.section
(** use Lwt_log.Section.set_level in order to set the log level *)

val run :
   ?continue:bool
  -> ?full_rewrite:bool
  -> regexp:string
  -> string
  -> unit
  -> Ocsigen_server.instruction
(** [run ~realm ~auth ()] makes it possible to use this extension without
    configuration file.  *)
