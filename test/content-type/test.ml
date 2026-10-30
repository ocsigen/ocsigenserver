(* A server given a mime.types file with ~mimefile, and serving a directory.
   The file must apply to the host given to [start]. With --no-charset, the
   server has no default charset. The paths that are not in the directory
   are answered with www/README, by [Ocsigen.Response.respond_file] without
   a content type. *)

let readme _vh _config_info _path = function
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return Ocsigen.Extensions.Ext_do_nothing
  | Ocsigen.Extensions.Req_not_found _ ->
      Lwt.return
        (Ocsigen.Extensions.Ext_found
           (fun () -> Ocsigen.Response.respond_file "www/README")
        )

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
    [Ocsigen.Server.host [Staticmod.run ~dir:"www" (); readme]]
