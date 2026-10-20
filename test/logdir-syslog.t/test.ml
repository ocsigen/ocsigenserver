(* A server started by Ocsigen.Server.start with a log directory, and syslog
   or not. Its pid file is in a missing directory: a server that passes the
   check of the log destination stops there, before opening any log. *)

let () =
  let syslog_facility =
    match Sys.argv with
    | [| _; "syslog" |] -> Some Syslog_message.Local0
    | [| _; "nosyslog" |] -> None
    | _ -> invalid_arg "usage: test.exe (syslog | nosyslog)"
  in
  Ocsigen.Server.start
    ~ports:[ (`Unix "./local.sock", 0) ]
    ~logdir:"log" ~syslog_facility ~uploaddir:None ~usedefaulthostname:true
    ~pidfile:"missing/server.pid"
    [ Ocsigen.Server.host [] ]
