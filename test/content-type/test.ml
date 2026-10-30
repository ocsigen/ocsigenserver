(* A server given a mime.types file with ~mimefile, and serving a directory.
   The host is evaluated before [start] sets the file, which must still
   apply to it. The paths that are not in the directory are answered with
   www/README, by [Ocsigen.Response.respond_file] without a content type. *)

let readme _vh _config_info _path = function
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return Ocsigen.Extensions.Ext_do_nothing
  | Ocsigen.Extensions.Req_not_found _ ->
      Lwt.return
        (Ocsigen.Extensions.Ext_found
           (fun () -> Ocsigen.Response.respond_file "www/README"))

let () =
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ~logdir:"log" ~uploaddir:None ~command_pipe:"local.cmd" ~mimefile:"my.types"
    [Ocsigen.Server.host [Staticmod.run ~dir:"www" (); readme]]
