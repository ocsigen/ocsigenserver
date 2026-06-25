(** Staticmod: serve static files *)

(** If you want to use this extension with Ocsigen Server's configuration file, 
   have a look at the {{!page-"staticmod"}manual page}.
   If you are using Ocsigen Server as a library, use the interface described
   here.
*)

(**
   This module belongs to ocamlfind package
   [ocsigenserver.ext.staticmod].
*)

(** Example of use:
{[
let _ =
   Ocsigen.Server.start
     [ Ocsigen.Server.host ~regexp:".*" [ Staticmod.run ~dir:"static" () ]]
]}
 *)

val run :
   ?dir:string
  -> ?regexp:string
  -> ?dest:string
  -> ?code:string
  -> ?cache:int
  -> ?root:string
  -> unit
  -> Ocsigen.Server.instruction
(** Run static mod on a specific directory. 
    Call this if you want to run Ocsigen Server without configuration file.
    The optional parameter correspond to the options of the configuration
    file described {{!page-"staticmod"}here}.*)

val section : Logs.src
