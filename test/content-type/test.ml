(* A server given a mime.types file with ~mimefile, and serving a directory.
   The host is evaluated before [start] sets the file, which must still
   apply to it. *)

let () =
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ~logdir:"log" ~uploaddir:None ~command_pipe:"local.cmd" ~mimefile:"my.types"
    [Ocsigen.Server.host [Staticmod.run ~dir:"www" ()]]
