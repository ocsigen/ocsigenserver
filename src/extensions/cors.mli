(** Cross-Origin Resource Sharing *)

(** If you want to use this extension with Ocsigen Server's configuration file, 
+   have a look at the {% <<a_manual chapter="cors"|manual page>>%}.
+   If you are using Ocsigen Server as a library, use the interface described
+   here. Each of these functions behaves exactly as its configuration file
    counterpart. 
+*)

(**
This module belongs to ocamlfind package
   [ocsigenserver.ext.cors].
*)

val run :
   ?credentials:bool
  -> ?max_age:int
  -> ?exposed_headers:string list
  -> ?methods:Cohttp.Code.meth list
  -> unit
  -> Ocsigen_server.instruction
(** [run] makes it possible to use this extension without
    configuration file.  *)
