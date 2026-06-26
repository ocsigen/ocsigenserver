(** Securityheaders: add common security-related response headers *)

(** If you want to use this extension with Ocsigen Server's configuration file,
    have a look at the {{!page-"securityheaders"}manual page}. If you are using
    Ocsigen Server as a library, use the interface described here.
*)

(**
   This module belongs to ocamlfind package
   [ocsigenserver.ext.securityheaders].
*)

(** Example of use:
{[
let _ =
   Ocsigen.Server.start
     [ Ocsigen.Server.host
         [ Staticmod.run ~dir:"static" ()
         ; Securityheaders.run () ]]
]}
    (the filter must come after the extension whose responses it decorates).
 *)

val run :
   ?nosniff:bool
  -> ?frame_options:string option
  -> ?hsts:string option
  -> ?content_security_policy:string
  -> unit
  -> Ocsigen.Server.instruction
(** [run ()] adds, to every response that does not already set them, the
    headers [X-Content-Type-Options: nosniff], [X-Frame-Options] and
    [Strict-Transport-Security]. Each can be customised or disabled:
    [~nosniff:false] omits the first; [~frame_options:None] (or
    [~hsts:None]) omits the corresponding header, [~frame_options:(Some v)]
    sets it to [v]. A [Content-Security-Policy] is added only when
    [~content_security_policy] is given (it has no safe default). *)
