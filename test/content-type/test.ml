(* A server given a mime.types file with ~mimefile, and serving a directory.
   The host is evaluated before [start] sets the file, which must still
   apply to it. With --no-charset, the server has no default charset. *)

let () =
  let default_charset =
    match Sys.argv with
    | [|_|] -> None
    | [|_; "--no-charset"|] -> Some None
    | _ -> invalid_arg "usage: test.exe [--no-charset]"
  in
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ~logdir:"log" ~uploaddir:None ~command_pipe:"local.cmd" ~mimefile:"my.types"
    ?default_charset
    [Ocsigen.Server.host [Staticmod.run ~dir:"www" ()]]
