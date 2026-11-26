(* Two virtual hosts given with Ocsigen.Server.host ~regexp. *)

let () =
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ~logdir:"log" ~uploaddir:None ~command_pipe:"local.cmd"
    [ Ocsigen.Server.host ~regexp:"admin\\.test$" [Staticmod.run ~dir:"admin" ()]
    ; Ocsigen.Server.host [Staticmod.run ~dir:"www" ()] ]
