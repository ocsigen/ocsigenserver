(* A static server with an index file and directory listing enabled, used to
   exercise Staticmod's path resolution. *)
let () =
  Ocsigen.Config.set_global_log_level (Some Logs.Error);
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ~logdir:"log" ~datadir:"data" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd" ~default_charset:(Some "utf-8")
    [ Ocsigen.Server.host ~default_directory_index:["index.html"]
        ~list_directory_content:true
        [Staticmod.run ~dir:"www" ()] ]
