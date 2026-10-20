(* A server that decodes the files of multipart requests into "uploads".
   It answers with the field, the name and whether the temporary file
   exists. A field named "keep" is moved to "kept", as an application does
   to keep a file. On the path "early", it answers without waiting for the
   files. Its info messages go to the standard output. *)

open Lwt.Infix

let describe (field, {Ocsigen.Request.tmp_filename; raw_original_filename; _}) =
  if field = "keep"
  then Sys.rename tmp_filename (Filename.concat "kept" raw_original_filename);
  Printf.sprintf "%s %s %b\n" field raw_original_filename
    (Sys.file_exists tmp_filename)

let upload : Ocsigen.Server.instruction =
 fun _ _ _ -> function
  | Ocsigen.Extensions.Req_not_found (err, {Ocsigen.Extensions.request_info; _})
    -> (
    match Ocsigen.Request.files request_info (Some "uploads") None with
    | None -> Lwt.return (Ocsigen.Extensions.Ext_next err)
    | Some _ when Ocsigen.Request.path_string request_info = "early" ->
        Lwt.return
          (Ocsigen.Extensions.Ext_found
             (fun () ->
               Lwt.return
                 (Ocsigen.Response.respond_string ~status:`OK ~body:"early\n" ())))
    | Some files ->
        files >|= fun files ->
        let body = String.concat "" (List.map describe files) in
        Ocsigen.Extensions.Ext_found
          (fun () ->
            Lwt.return (Ocsigen.Response.respond_string ~status:`OK ~body ())))
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return Ocsigen.Extensions.Ext_do_nothing

let () =
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ~logdir:"log" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd" ~veryverbose:()
    [Ocsigen.Server.host [upload]]
