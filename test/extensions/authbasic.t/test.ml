(* Authbasic: protect a site with HTTP Basic authentication. *)
let () =
  Ocsigen.Config.set_global_log_level (Some Logs.Error);
  Ocsigen.Server.start
    ~ports:[ (`Unix "./local.sock", 0) ]
    ~logdir:"log" ~datadir:"data" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd" ~default_charset:(Some "utf-8")
    [
      Ocsigen.Server.host
        [
          Authbasic.run ~realm:"test"
            ~auth:(fun u p -> Lwt.return (u = "user" && p = "pass"))
            ();
          Staticmod.run ~dir:"." ();
        ];
    ]
