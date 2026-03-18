let () =
  Ocsigen_config.set_global_log_level (Some Logs.Debug);
  Ocsigen_server.start
    ~ports:[ (`Unix "./local.sock", 0) ]
    ~veryverbose:() ~debugmode:true ~logdir:"log" ~datadir:"data"
    ~uploaddir:None ~usedefaulthostname:true ~command_pipe:"local.cmd"
    ~default_charset:(Some "utf-8")
    [
      Ocsigen_server.host
        [ Staticmod.run ~dir:"." (); Deflatemod.run ~mode:(`All_but []) () ];
    ]
