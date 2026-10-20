let () =
  Ocsigen.Config.set_global_log_level (Some Logs.Debug);
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ~veryverbose:() ~debugmode:true ~logdir:"log" ~uploaddir:None
    ~usedefaulthostname:true ~command_pipe:"local.cmd"
    ~default_charset:(Some "utf-8")
    [ Ocsigen.Server.host
        [ Staticmod.run ~dir:"." ()
        ; Ocsigen.Server.site ["missing"]
            [Staticmod.run ~code:"404" ~regexp:".*" ~dest:"error.html" ()]
        ; Deflatemod.run ~mode:(`All_but []) () ] ]
