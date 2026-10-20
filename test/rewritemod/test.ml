(* A server that rewrites old/... to new/..., then serves the files of
   "www". *)

let () =
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ~logdir:"log" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd"
    [ Ocsigen.Server.host
        [ Rewritemod.run ~regexp:"old/(.*)" "new/\\1" ()
        ; Staticmod.run ~dir:"www" () ] ]
