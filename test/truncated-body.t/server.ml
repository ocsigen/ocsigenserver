(* Server for the truncated body test: answers the length of the parameter [a]
   of a URL-encoded form, or else the length of the whole body. *)

open Lwt.Infix

let length request_info =
  match Ocsigen.Request.post_params request_info None None with
  | Some params ->
      params >|= fun params ->
      Option.fold ~none:(-1) ~some:String.length (List.assoc_opt "a" params)
  | None ->
      Cohttp_lwt.Body.to_string (Ocsigen.Request.body request_info)
      >|= String.length

let resources _vh _config_info _path = function
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return Ocsigen.Extensions.Ext_do_nothing
  | Ocsigen.Extensions.Req_not_found (_, {Ocsigen.Extensions.request_info; _})
    ->
      length request_info >>= fun length ->
      Lwt.return
        (Ocsigen.Extensions.Ext_found
           (fun () ->
             Lwt.return
               (Ocsigen.Response.respond_string ~status:`OK
                  ~body:(string_of_int length) ())))

let () =
  Ocsigen.Server.start
    ~ports:[`All, 8079]
    ~logdir:"log" ~datadir:"data" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd" ~default_charset:(Some "utf-8")
    [Ocsigen.Server.host [resources]]
