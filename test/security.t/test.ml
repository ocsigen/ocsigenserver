(* End-to-end checks for the security hardening:
   - F4: a handler error must not leak the OCaml exception in the response
     body; the client gets the generic HTTP status reason phrase.
   - F8: the command pipe must be created with mode 0o600.

   The [boom] instruction intercepts every request and fails with an
   exception carrying a "secret" message that must never reach the client. *)

let boom _vh _config_info _path _request_state =
  Lwt.fail (Failure "secret internal detail that must not leak to the client")

let () =
  Ocsigen.Server.start
    ~ports:[ (`Unix "./local.sock", 0) ]
    ~logdir:"log" ~datadir:"data" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd" ~default_charset:(Some "utf-8")
    [ Ocsigen.Server.host [ boom ] ]
