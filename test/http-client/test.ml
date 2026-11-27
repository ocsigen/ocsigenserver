(* A server whose only instruction sends a request with
   [Cohttp_lwt_unix.Client] to /malformed on the target server
   (revproxy-forwarding/raw.exe) at the port given as argument, and answers
   with its outcome: for /malformed, a GET, which fails as the target does not
   answer in HTTP; for /unreadable-body, a POST whose body fails to be read.
   Other paths send no request. *)

open Lwt.Infix

let send target = function
  | "/malformed" -> Some (fun () -> Cohttp_lwt_unix.Client.get target)
  | "/unreadable-body" ->
      let body = `Stream (Lwt_stream.from (fun () -> Lwt.fail Exit)) in
      Some (fun () -> Cohttp_lwt_unix.Client.post ~body target)
  | _ -> None

let outcome target path =
  match send target path with
  | None -> Lwt.return "no request"
  | Some send ->
      Lwt.catch
        (fun () ->
          send () >>= fun (response, body) ->
          Cohttp_lwt.Body.drain_body body >|= fun () ->
          Printf.sprintf "status %d"
            (Cohttp.Code.code_of_status (Cohttp.Response.status response))
        )
        (function
          | Exit -> Lwt.return "failed: Exit"
          | Failure _ -> Lwt.return "failed: Failure"
          | exn -> Lwt.return ("failed: " ^ Printexc.to_string exn)
          )

let respond target _vh _config_info _path = function
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return Ocsigen.Extensions.Ext_do_nothing
  | Ocsigen.Extensions.Req_not_found (_, {Ocsigen.Extensions.request_info; _})
    ->
      Lwt.return
        (Ocsigen.Extensions.Ext_found
           (fun () ->
             outcome target (Uri.path (Ocsigen.Request.uri request_info))
             >|= fun body ->
             Ocsigen.Response.respond_string ~status:`OK ~body:(body ^ "\n") ()
           )
        )

let () =
  let target =
    match Sys.argv with
    | [|_; port|] -> Uri.of_string ("http://127.0.0.1:" ^ port ^ "/malformed")
    | _ -> failwith "usage: test.exe TARGET_PORT"
  in
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ~logdir:"log" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd"
    [Ocsigen.Server.host [respond target]]
