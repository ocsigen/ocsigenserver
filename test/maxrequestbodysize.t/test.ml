(* End-to-end checks for <maxrequestbodysize>, set here to 1000 bytes. The
   [echo] instruction reads the whole body of a request and answers with its
   length or, for /params, decodes its POST parameters (urlencoded or
   multipart) and answers with their number and total size. *)

let respond body =
  Lwt.return
    (Ocsigen.Extensions.Ext_found
       (fun () ->
         Lwt.return (Ocsigen.Response.respond_string ~status:`OK ~body ())))

let echo _vh _config_info _path = function
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return Ocsigen.Extensions.Ext_do_nothing
  | Ocsigen.Extensions.Req_not_found (_, {Ocsigen.Extensions.request_info; _})
    -> (
      match Ocsigen.Request.path request_info with
      | [ "params" ] -> (
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
      | _ ->
          Lwt.bind
            (Cohttp_lwt.Body.to_string (Ocsigen.Request.body request_info))
            (fun body ->
              respond (Printf.sprintf "%d bytes" (String.length body))))

let () =
  Ocsigen.Server.start
    ~ports:[ (`Unix "./local.sock", 0) ]
    ~logdir:"log" ~datadir:"data" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd" ~maxrequestbodysize:(Some 1000L)
    [ Ocsigen.Server.host [ echo ] ]
