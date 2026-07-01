(* Rewritemod: rewrite the request path before it reaches other extensions. *)
let () =
  Ocsigen.Config.set_global_log_level (Some Logs.Error);
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ~logdir:"log" ~datadir:"data" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd" ~default_charset:(Some "utf-8")
    [ Ocsigen.Server.host
        [ Rewritemod.run ~continue:true ~regexp:"alias/(.*)" "\\1" ()
        ; Staticmod.run ~dir:"." () ] ]
