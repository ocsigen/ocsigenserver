(* A server listening on a Unix-domain socket only, which has no port, and
   serving a directory. *)

let () =
  Ocsigen.Server.start
    ~ports:[ (`Unix "./local.sock", 0) ]
    ~logdir:"log" ~datadir:"data" ~uploaddir:None ~command_pipe:"local.cmd"
    [ Ocsigen.Server.host [ Staticmod.run ~dir:"www" () ] ]
