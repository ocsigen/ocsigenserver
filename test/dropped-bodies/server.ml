(* A server that answers file.txt to the paths that end with "file", then
   drops this response as the first segment of the path says:
   - next: the next extension is tried ([Ext_next]);
   - stop: the server answers 403 ([Ext_stop_all]);
   - fail: the extension fails;
   - fail-later: the page that the extension gives fails;
   - fail-stop: the same, with [Ext_found_stop].
   The other paths keep the response. [open-files] gives the number of open
   files (../fd-count). The timeout of the clients is 1 second. *)

let file _vh _config_info _path = function
  | Ocsigen.Extensions.Req_not_found (err, {Ocsigen.Extensions.request_info; _})
    -> (
    match List.rev (Ocsigen.Request.path request_info) with
    | "file" :: _ ->
        Lwt.return
          (Ocsigen.Extensions.Ext_found
             (fun () -> Ocsigen.Response.respond_file "file.txt"))
    | _ -> Lwt.return (Ocsigen.Extensions.Ext_next err))
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return Ocsigen.Extensions.Ext_do_nothing

let drop _vh _config_info _path = function
  | Ocsigen.Extensions.Req_found ({Ocsigen.Extensions.request_info; _}, _) -> (
    match Ocsigen.Request.path request_info with
    | "next" :: _ -> Lwt.return (Ocsigen.Extensions.Ext_next `Not_found)
    | "stop" :: _ ->
        Lwt.return
          (Ocsigen.Extensions.Ext_stop_all (Ocsigen_cookie_map.empty, `Forbidden))
    | "fail" :: _ -> Lwt.fail (Failure "drop")
    | "fail-later" :: _ ->
        Lwt.return
          (Ocsigen.Extensions.Ext_found (fun () -> Lwt.fail (Failure "drop")))
    | "fail-stop" :: _ ->
        Lwt.return
          (Ocsigen.Extensions.Ext_found_stop
             (fun () -> Lwt.fail (Failure "drop")))
    | _ -> Lwt.return Ocsigen.Extensions.Ext_do_nothing)
  | Ocsigen.Extensions.Req_not_found (err, _) ->
      Lwt.return (Ocsigen.Extensions.Ext_next err)

let () =
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ~logdir:"log" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd" ~client_timeout:1
    [Ocsigen.Server.host [Fd_count.instruction; file; drop]]
