(* A server that decodes the form data of POST requests, with files in
   "uploads", and answers with its description (see [Describe]). *)

open Lwt.Infix

let describe : Ocsigen.Server.instruction =
 fun _ _ _ -> function
  | Ocsigen.Extensions.Req_not_found (err, {Ocsigen.Extensions.request_info; _})
    -> (
    match
      ( Ocsigen.Request.post_params request_info (Some "uploads") None
      , Ocsigen.Request.files request_info (Some "uploads") None )
    with
    | Some params, Some files ->
        Lwt.both params files >|= fun post_data ->
        let body = Describe.post_data post_data in
        Ocsigen.Extensions.Ext_found
          (fun () ->
            Lwt.return (Ocsigen.Response.respond_string ~status:`OK ~body ()))
    | _ -> Lwt.return (Ocsigen.Extensions.Ext_next err))
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return Ocsigen.Extensions.Ext_do_nothing

let () =
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ~logdir:"log" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd"
    [Ocsigen.Server.host [describe]]
