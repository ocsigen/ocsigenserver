(* A server whose only instruction answers each path with a response whose
   framing the headers alone would get wrong. *)

let chunked = Ocsigen.Response.Body.make Cohttp.Transfer.Chunked

let response path =
  let open Cohttp.Header in
  match path with
  | "range-chunked" ->
      Some
        (Ocsigen.Response.respond
           ~headers:(of_list ["content-range", "bytes 0-4/10"])
           ~status:`Partial_content
           ~body:(chunked (fun write -> write "01234"))
           ())
  | "length-chunked" ->
      Some
        (Ocsigen.Response.respond
           ~headers:(of_list ["content-length", "99"])
           ~status:`OK
           ~body:(chunked (fun write -> write "hello"))
           ())
  | "range-fixed" ->
      Some
        (Ocsigen.Response.respond
           ~headers:(of_list ["content-range", "bytes 5-9/10"])
           ~status:`Partial_content
           ~body:(Ocsigen.Response.Body.of_string "56789")
           ())
  | "length-fixed" ->
      Some
        (Ocsigen.Response.respond
           ~headers:(of_list ["content-length", "99"])
           ~status:`OK
           ~body:(Ocsigen.Response.Body.of_string "world")
           ())
  | "no-content" -> Some (Ocsigen.Response.respond ~status:`No_content ())
  | "not-modified" ->
      Some
        (Ocsigen.Response.respond
           ~headers:(of_list ["content-length", "99"])
           ~status:`Not_modified ())
  | _ -> None

let respond _vh _config_info _path = function
  | Ocsigen.Extensions.Req_not_found (_, {Ocsigen.Extensions.request_info; _})
    -> (
    match response (Ocsigen.Request.sub_path_string request_info) with
    | Some response ->
        Lwt.return
          (Ocsigen.Extensions.Ext_found (fun () -> Lwt.return response))
    | None -> Lwt.return (Ocsigen.Extensions.Ext_next `Not_found))
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return (Ocsigen.Extensions.Ext_next `Not_found)

let () =
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ~logdir:"log" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd"
    [Ocsigen.Server.host [respond]]
