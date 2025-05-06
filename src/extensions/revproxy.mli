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

(** Example of use. Forward all requests to a given directory to the
same directory of another server running locally on another port.
We are using it in combination with
{% <<a_manual chapter="outputfilter"|Outputfilter>>%} to rewrite redirections.

{[
let _ =
   Ocsigen_server.start
     [ Ocsigen_server.host ~regexp:".*"
       [ Revproxy.run
           ~redirection:(Revproxy.create_redirection 
                           ~full_url:false
                           ~regexp:"(othersite/.* )"
                           ~keephost:true
                           "https://localhost:8123/\\1") 
           ()
        ; Outputfilter.run 
            ~mode:(`Rewrite (Ocsigen_header.Name.location,
                             "http://localhost:8123/(.* )",
                             "http://my.publicaddress.org/\\1"))
            ()
 ]]
]}
 *)

val section : Logs.src

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
