(* A server given settings for its hosts by Ocsigen.Server.start. OCaml
   evaluates the hosts before [start] sets them, and they must still apply to
   the hosts, unless a host gives its own. The path /settings answers with the
   settings of the host, and the other paths are the static files of
   directory www. *)

let pp_option pp fmt = function
  | None -> Format.pp_print_string fmt "none"
  | Some v -> pp fmt v

let pp_settings
      fmt
      { Ocsigen.Extensions.default_httpport
      ; charset_assoc
      ; uploaddir
      ; maxuploadfilesize
      ; _ }
  =
  Format.fprintf fmt
    "default port: %d@\ncharset: %s@\nuploaddir: %a@\nmaxuploadfilesize: %a@\n"
    default_httpport
    (Ocsigen_http.Charset_mime.default_charset charset_assoc)
    (pp_option Format.pp_print_string)
    uploaddir
    (pp_option (fun fmt -> Format.fprintf fmt "%Ld"))
    maxuploadfilesize

let settings _vh _config_info _path = function
  | Ocsigen.Extensions.Req_not_found
      (_, {Ocsigen.Extensions.request_info; request_config})
    when Ocsigen.Request.sub_path_string request_info = "settings" ->
      let body = Format.asprintf "%a" pp_settings request_config in
      Lwt.return
        (Ocsigen.Extensions.Ext_found
           (fun () ->
             Lwt.return (Ocsigen.Response.respond_string ~status:`OK ~body ())))
  | Ocsigen.Extensions.Req_not_found (status, _) ->
      Lwt.return (Ocsigen.Extensions.Ext_next status)
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return Ocsigen.Extensions.Ext_do_nothing

let instructions = [settings; Staticmod.run ~dir:"www" ()]

(* The server also listens on the TCP port given on the command line, which is
   then the default port of the hosts: the Unix-domain socket has none. *)
let () =
  Ocsigen.Server.start
    ~ports:
      [ `Unix "./local.sock", 0
      ; `IPv4 Unix.inet_addr_loopback, int_of_string Sys.argv.(1) ]
    ~logdir:"log" ~command_pipe:"local.cmd"
    ~default_charset:(Some "iso-8859-15") ~uploaddir:(Some "upload")
    ~maxuploadfilesize:(Some 1234L)
    [ Ocsigen.Server.host ~regexp:"own"
        ~charset_assoc:
          (Ocsigen_http.Charset_mime.empty_charset_assoc ~default:"utf-8" ())
        ~uploaddir:None instructions
    ; Ocsigen.Server.host instructions ]
