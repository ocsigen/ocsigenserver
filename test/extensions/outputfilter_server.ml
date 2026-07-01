(* Outputfilter: add a header to every response. *)
let () =
  Ocsigen.Config.set_global_log_level (Some Logs.Error);
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ~logdir:"log" ~datadir:"data" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd" ~default_charset:(Some "utf-8")
    [ Ocsigen.Server.host
        [ Staticmod.run ~dir:"." ()
        ; Outputfilter.run
            ~mode:
              (`Add (Ocsigen_http.Header.Name.of_string "x-test", "hello", None))
            () ] ]
