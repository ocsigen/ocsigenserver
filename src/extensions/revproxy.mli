(** Revproxy: Forward a request to another Web server *)

(** If you want to use this extension with Ocsigen Server's configuration file,
    have a look at the {% <<a_manual chapter="revproxy"|manual page>>%}.
    If you are using Ocsigen Server as a library, use the interface described
    here. Each of these functions behaves exactly as its configuration file
    counterpart.
*)

(**
This module belongs to ocamlfind package
   [ocsigenserver.ext.revproxy].
*)

(** Example of use:
{[
let _ =
   Ocsigen_server.start
     [ Ocsigen_server.host ~regexp:".*"
       [ Ocsigen_server.site ["blah"]
        [ Revproxy.run
            ~redirection:(Revproxy.create_redirection ) ]]]
]}
 *)

val section : Lwt_log_core.section
(** use Lwt_log.Section.set_level in order to set the log level *)

type redirection

val create_redirection :
   ?full_url:bool
  -> ?pipeline:bool
  -> ?keephost:bool
  -> regexp:string
  -> string
  -> redirection

val run : redirection:redirection -> unit -> Ocsigen_server.instruction
(** [run ~redirection ()] makes it possible to use this extension without
    configuration file.  *)
