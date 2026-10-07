(* End-to-end checks for the size limit of request bodies decoded in memory
   (<maxrequestbodysizeinmemory>, 1 MiB by default).

   The [echo_params] instruction decodes the POST parameters of every request
   and answers with their number and total size. *)

let respond body =
  Lwt.return
    (Ocsigen.Extensions.Ext_found
       (fun () ->
         Lwt.return (Ocsigen.Response.respond_string ~status:`OK ~body ())))

let echo_params _vh _config_info _path = function
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return Ocsigen.Extensions.Ext_do_nothing
  | Ocsigen.Extensions.Req_not_found (_, {Ocsigen.Extensions.request_info; _})
    -> (
      match Ocsigen.Request.post_params request_info None None with
      | None -> respond "no parameters"
      | Some params ->
          Lwt.bind params (fun params ->
            let size =
              List.fold_left
                (fun n (k, v) -> n + String.length k + String.length v)
                0 params
            in
            respond
              (Printf.sprintf "%d parameter(s), %d bytes"
                 (List.length params) size)))

let () =
  Ocsigen.Server.start
    ~ports:[(`Unix "./local.sock", 0)]
    ~logdir:"log" ~datadir:"data" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd" ~default_charset:(Some "utf-8")
    [Ocsigen.Server.host [echo_params]]
