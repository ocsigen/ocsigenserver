(* Redirectmod: redirect requests matching a regexp to another location. *)
let () =
  Ocsigen.Config.set_global_log_level (Some Logs.Error);
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ~logdir:"log" ~datadir:"data" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd" ~default_charset:(Some "utf-8")
    [ Ocsigen.Server.host
        [ Redirectmod.run
            ~redirection:
              (Redirectmod.create_redirection ~temporary:false ~full_url:false
                 ~regexp:"^old/(.*)$" "new/\\1")
            ()
        ; Staticmod.run ~dir:"." () ] ]
