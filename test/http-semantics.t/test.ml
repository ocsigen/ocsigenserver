(* A minimal static server used to exercise core HTTP behaviour. Logs are kept
   at the [Error] level so that the cram output only contains curl responses. *)
let () =
  Ocsigen.Config.set_global_log_level (Some Logs.Error);
  Ocsigen.Server.start
    ~ports:[ (`Unix "./local.sock", 0) ]
    ~logdir:"log" ~datadir:"data" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd" ~default_charset:(Some "utf-8")
    [ Ocsigen.Server.host [ Staticmod.run ~dir:"." () ] ]
