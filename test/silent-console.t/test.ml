(* A server whose logs go to the terminal, as with a server built with no
   default log directory, started in silent (-s) or daemon (-d) mode. It
   installs no Logs reporter: the server must report its errors anyway. *)

let () =
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
