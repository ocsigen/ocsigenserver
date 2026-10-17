(* A server whose logs go to the terminal, as with a server built with no
   default log directory, started in silent (-s) or daemon (-d) mode. *)

let () =
  (* As the ocsigenserver command does, so that the errors reach the terminal
     before the server configures its logging. *)
  Logs.set_reporter Ocsigen.Messages.stdio_reporter;
  let silent, daemon =
    match Sys.argv with
    | [| _; "-s" |] -> Some (), None
    | [| _; "-d" |] -> None, Some ()
    | _ -> invalid_arg "usage: test.exe (-s | -d)"
  in
  Ocsigen.Server.start
    ~ports:[ (`Unix "./local.sock", 0) ]
    ~logdir:"" ~datadir:"data" ~uploaddir:None ~usedefaulthostname:true
    ?silent ?daemon
    [ Ocsigen.Server.host [] ]
