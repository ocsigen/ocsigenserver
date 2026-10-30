(* A server that fails with an exception on /fail, and serves the files of
   "www" otherwise. *)

let fail _vh _config_info _path = function
  | Ocsigen.Extensions.Req_not_found (_, {Ocsigen.Extensions.request_info; _})
    when Ocsigen.Request.path request_info = ["fail"] ->
      Lwt.fail (Failure "test failure")
  | Ocsigen.Extensions.Req_found _ | Ocsigen.Extensions.Req_not_found _ ->
      Lwt.return Ocsigen.Extensions.Ext_do_nothing

let () =
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ~logdir:"log" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd"
    [Ocsigen.Server.host [fail; Staticmod.run ~dir:"www" ()]]
