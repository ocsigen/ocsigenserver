(* A server started in silent (-s) or daemon (-d) mode, with the log
   directory given as an optional second argument. Without it, its logs go to
   the terminal. It installs no Logs reporter: the server must report its
   errors anyway. *)

let usage () = invalid_arg "usage: test.exe (-s | -d) [LOGDIR]"

let () =
  let mode, logdir =
    match Sys.argv with
    | [|_; mode|] -> mode, None
    | [|_; mode; logdir|] -> mode, Some logdir
    | _ -> usage ()
  in
  let silent, daemon =
    match mode with
    | "-s" -> Some (), None
    | "-d" -> None, Some ()
    | _ -> usage ()
  in
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ?logdir ~uploaddir:None ~usedefaulthostname:true ~pidfile:"server.pid"
    ?silent ?daemon
    [Ocsigen.Server.host []]
