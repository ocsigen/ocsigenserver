(** Securityheaders: add common security-related response headers *)

(** If you want to use this extension with Ocsigen Server's configuration file,
    have a look at the {{!page-"securityheaders"}manual page}. If you are using
    Ocsigen Server as a library, use the interface described here.
*)

(**
   This module belongs to ocamlfind package
   [ocsigenserver.ext.securityheaders].
*)

(** {b Ordering matters.} This is a response filter: it only decorates
    responses that an earlier extension has produced. Place it {e after} the
    extension whose responses it should cover (Staticmod, Eliom, ...). If it is
    placed before, it sees no response and silently adds nothing.

    Example of use:
{[
let _ =
   Ocsigen.Server.start
     [ Ocsigen.Server.host
         [ Staticmod.run ~dir:"static" ()
         ; Securityheaders.run () ]]
]}
 *)

val run :
   ?nosniff:bool
  -> ?frame_options:string option
  -> ?referrer_policy:string option
  -> ?hsts:string option
  -> ?content_security_policy:string
  -> unit
  -> Ocsigen.Server.instruction
(** [run ()] adds, to every response that does not already set them, three
    safe-by-default headers: [X-Content-Type-Options: nosniff],
    [X-Frame-Options: SAMEORIGIN] and
    [Referrer-Policy: strict-origin-when-cross-origin]. Each can be customised
    ([~frame_options:(Some "DENY")]) or disabled ([~nosniff:false],
    [~frame_options:None]).

    [Strict-Transport-Security] and [Content-Security-Policy] are {b opt-in}
    (both default to [None]) and must be set explicitly, e.g.
    [~hsts:(Some "max-age=15552000; includeSubDomains")]. HSTS is only honoured
    over HTTPS and is a sticky commitment: [includeSubDomains] forces every
    subdomain of the registrable domain to HTTPS for the whole [max-age], so
    enable it only when the whole domain is HTTPS-only. A [Content-Security-Policy]
    has no safe generic default; set one tailored to your application. *)
