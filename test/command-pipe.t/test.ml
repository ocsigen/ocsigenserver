(* A server with a command pipe, used to check how an existing pipe path is
   handled. It has no instruction, so every request gets a 404. *)

let () =
  Ocsigen.Server.start
    ~ports:[ (`Unix "./local.sock", 0) ]
    ~logdir:"log" ~datadir:"data" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd"
    [ Ocsigen.Server.host [] ]
