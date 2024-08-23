(** Outputfilter: Rewrite some part of the output *)

(** If you want to use this extension with Ocsigen Server's configuration file,
    have a look at the {% <<a_manual chapter="outputfilter"|manual page>>%}.
    If you are using Ocsigen Server as a library, use the interface described
    here. Each of these functions behaves exactly as its configuration file
    counterpart. 
+*)

(**
This module belongs to ocamlfind package
   [ocsigenserver.ext.outputfilter].
*)

(** See an example of use on the API documentation of {!Revproxy}. *)

val run :
   mode:
     [ `Rewrite of Ocsigen_header.Name.t * string * string
     | `Add of Ocsigen_header.Name.t * string * bool option
     | `Code of Cohttp.Code.status ]
  -> unit
  -> Ocsigen_server.instruction
(** [run ~mode ()] makes it possible to use this extension without
    configuration file.  *)
