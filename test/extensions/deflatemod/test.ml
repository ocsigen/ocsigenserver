(* The number of regular files open in the server, on Linux and macOS: the
   connections, which come and go, are not counted. *)
let open_files _vh _config_info _path = function
  | Ocsigen.Extensions.Req_not_found (err, {Ocsigen.Extensions.request_info; _})
    ->
      if Ocsigen.Request.sub_path_string request_info = "open-files"
      then
        let is_file fd =
          match Unix.stat ("/dev/fd/" ^ fd) with
          | {Unix.st_kind = Unix.S_REG; _} -> true
          | _ -> false
          | exception Unix.Unix_error _ -> false
        in
        let n =
          List.length
            (List.filter is_file (Array.to_list (Sys.readdir "/dev/fd")))
        in
        Lwt.return
          (Ocsigen.Extensions.Ext_found
             (fun () ->
               Lwt.return
                 (Ocsigen.Response.respond_string ~status:`OK
                    ~body:(string_of_int n) ())))
      else Lwt.return (Ocsigen.Extensions.Ext_next err)
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return Ocsigen.Extensions.Ext_do_nothing

let () =
  Ocsigen.Config.set_global_log_level (Some Logs.Debug);
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ~veryverbose:() ~debugmode:true ~logdir:"log" ~uploaddir:None
    ~usedefaulthostname:true ~command_pipe:"local.cmd"
    ~default_charset:(Some "utf-8")
    [ Ocsigen.Server.host
        [ open_files
        ; Staticmod.run ~dir:"." ()
        ; Ocsigen.Server.site ["missing"]
            [Staticmod.run ~code:"404" ~regexp:".*" ~dest:"error.html" ()]
        ; Deflatemod.run ~mode:(`All_but []) () ] ]
