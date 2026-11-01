(* A server that answers file.txt to the paths that end with "file", then
   drops this response as the first segment of the path says:
   - next: the next extension is tried ([Ext_next]);
   - continue: the same, with [Ext_continue_with];
   - retry: the request is retried as keep/file ([Ext_retry_with]);
   - stop, stop-site, stop-host: the server answers 403 ([Ext_stop_all],
     [Ext_stop_site], [Ext_stop_host]);
   - fail: the extension fails;
   - fail-later: the page that the extension gives fails;
   - fail-stop, fail-continue: the same, with [Ext_found_stop] and
     [Ext_found_continue_with].
   The other paths keep the response. When the second segment is "nested",
   the response is dropped by an extension composed in another one
   ([Ext_sub_result]). [open-files] gives the number of open files
   (../fd-count). *)

let file _vh _config_info _path = function
  | Ocsigen.Extensions.Req_not_found (err, {Ocsigen.Extensions.request_info; _})
    -> (
    match List.rev (Ocsigen.Request.path request_info) with
    | "file" :: _ ->
        Lwt.return
          (Ocsigen.Extensions.Ext_found
             (fun () -> Ocsigen.Response.respond_file "file.txt")
          )
    | _ -> Lwt.return (Ocsigen.Extensions.Ext_next err)
  )
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return Ocsigen.Extensions.Ext_do_nothing

let forbidden = Ocsigen_cookie_map.empty, `Forbidden
let failing () = Lwt.fail (Failure "drop")

let drop _vh _config_info _path = function
  | Ocsigen.Extensions.Req_found
      (({Ocsigen.Extensions.request_info; _} as request), _) -> (
    match Ocsigen.Request.path request_info with
    | "next" :: _ -> Lwt.return (Ocsigen.Extensions.Ext_next `Not_found)
    | "continue" :: _ ->
        Lwt.return
          (Ocsigen.Extensions.Ext_continue_with
             (request, Ocsigen_cookie_map.empty, `Not_found)
          )
    | "retry" :: _ ->
        let uri =
          Uri.with_path (Ocsigen.Request.uri request_info) "/keep/file"
        in
        let request_info = Ocsigen.Request.update ~uri request_info in
        Lwt.return
          (Ocsigen.Extensions.Ext_retry_with
             ({request with request_info}, Ocsigen_cookie_map.empty)
          )
    | "stop" :: _ -> Lwt.return (Ocsigen.Extensions.Ext_stop_all forbidden)
    | "stop-site" :: _ -> Lwt.return (Ocsigen.Extensions.Ext_stop_site forbidden)
    | "stop-host" :: _ -> Lwt.return (Ocsigen.Extensions.Ext_stop_host forbidden)
    | "fail" :: _ -> failing ()
    | "fail-later" :: _ -> Lwt.return (Ocsigen.Extensions.Ext_found failing)
    | "fail-stop" :: _ -> Lwt.return (Ocsigen.Extensions.Ext_found_stop failing)
    | "fail-continue" :: _ ->
        Lwt.return (Ocsigen.Extensions.Ext_found_continue_with failing)
    | _ -> Lwt.return Ocsigen.Extensions.Ext_do_nothing
  )
  | Ocsigen.Extensions.Req_not_found (err, _) ->
      Lwt.return (Ocsigen.Extensions.Ext_next err)

let nested vh config_info path = function
  | Ocsigen.Extensions.Req_found ({Ocsigen.Extensions.request_info; _}, _) -> (
    match Ocsigen.Request.path request_info with
    | _ :: "nested" :: _ ->
        Lwt.return
          (Ocsigen.Extensions.Ext_sub_result
             (Ocsigen.Extensions.compose [drop vh config_info path])
          )
    | _ -> Lwt.return Ocsigen.Extensions.Ext_do_nothing
  )
  | Ocsigen.Extensions.Req_not_found (err, _) ->
      Lwt.return (Ocsigen.Extensions.Ext_next err)

let () =
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ~logdir:"log" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd"
    [Ocsigen.Server.host [Fd_count.instruction; file; nested; drop]]
