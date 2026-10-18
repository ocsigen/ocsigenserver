(* A server whose only instruction answers with status code 421 (Misdirected
   Request), which Cohttp has no constructor for, as a reverse proxy may relay
   from its target. *)

let respond _vh _config_info _path _request_state =
  Lwt.return
    (Ocsigen.Extensions.Ext_found
       (fun () ->
         Lwt.return
           (Ocsigen.Response.respond_string ~status:(`Code 421) ~body:"x" ())))

let () =
  Ocsigen.Server.start
    ~ports:[ (`Unix "./local.sock", 0) ]
    ~logdir:"log" ~datadir:"data" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd"
    [ Ocsigen.Server.host [ respond ] ]
