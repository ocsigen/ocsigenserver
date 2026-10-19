(* A server started in silent (-s) or daemon (-d) mode, with the log
   directory given as second argument. Its logs go to the terminal when that
   directory is empty, as with a server built with no default log directory.
   It installs no Logs reporter: the server must report its errors anyway. *)

let () =
  let silent, daemon, logdir =
    match Sys.argv with
    | [| _; "-s"; logdir |] -> Some (), None, logdir
    | [| _; "-d"; logdir |] -> None, Some (), logdir
    | _ -> invalid_arg "usage: test.exe (-s | -d) LOGDIR"
  in
  Ocsigen.Server.start
    ~ports:[ (`Unix "./local.sock", 0) ]
    ~logdir ~uploaddir:None ~usedefaulthostname:true
    ~pidfile:"server.pid" ?silent ?daemon
    [ Ocsigen.Server.host [] ]
