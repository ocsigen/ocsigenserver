(* A server with a command pipe and an extra order, "test:fail", that always
   fails. It has no instruction, so every request gets a 404. *)

let () =
  Ocsigen.Command.register_command_function ~prefix:"test" (fun _ _ ->
    Lwt.fail (Failure "this order always fails"))

let () =
  Ocsigen.Server.start
    ~ports:[ (`Unix "./local.sock", 0) ]
    ~logdir:"log" ~datadir:"data" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd"
    [ Ocsigen.Server.host [] ]
