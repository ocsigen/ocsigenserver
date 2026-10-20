(* A server that serves "www", and rewrites "plain" into "html" in the
   Content-Type of its responses, with Outputfilter.run. *)

let () =
  Ocsigen.Server.start
    ~ports:[ (`Unix "./local.sock", 0) ]
    ~logdir:"log" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd"
    [ Ocsigen.Server.host
        [ Staticmod.run ~dir:"www" ()
        ; Outputfilter.run
            ~mode:
              (`Rewrite (Ocsigen_http.Header.Name.content_type, "plain", "html"))
            () ] ]
