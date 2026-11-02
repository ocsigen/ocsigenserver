let open_files () =
  let is_file fd =
    match Unix.stat ("/dev/fd/" ^ fd) with
    | {Unix.st_kind = Unix.S_REG; _} -> true
    | _ -> false
    (* The descriptor that read the directory is closed already. *)
    | exception Unix.Unix_error _ -> false
  in
  List.length (List.filter is_file (Array.to_list (Sys.readdir "/dev/fd")))

let instruction _vh _config_info _path = function
  | Ocsigen.Extensions.Req_not_found (err, {Ocsigen.Extensions.request_info; _})
    ->
      if Ocsigen.Request.sub_path_string request_info = "open-files"
      then
        let n = open_files () in
        Lwt.return
          (Ocsigen.Extensions.Ext_found
             (fun () ->
               Lwt.return
                 (Ocsigen.Response.respond_string ~status:`OK
                    ~body:(string_of_int n) ())))
      else Lwt.return (Ocsigen.Extensions.Ext_next err)
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return Ocsigen.Extensions.Ext_do_nothing
