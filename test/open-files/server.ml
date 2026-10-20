(* A server on a Unix-domain socket that answers the maximum number of
   connections in effect, and its soft limit on open files. With the argument
   [explicit] or [fitted], <maxconnected> is set to 1000; with [default] or [tiny], it is
   left to its default. *)

let answer () =
  Printf.sprintf "%d connections, soft limit %s"
    (Ocsigen.Config.get_max_number_of_connections ())
    (match Ocsigen.Open_files.limits () with
    | Some (soft, _) -> string_of_int soft
    | None -> "none")

let resources _vh _config_info _path = function
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return Ocsigen.Extensions.Ext_do_nothing
  | Ocsigen.Extensions.Req_not_found _ ->
      Lwt.return
        (Ocsigen.Extensions.Ext_found
           (fun () ->
             Lwt.return
               (Ocsigen.Response.respond_string ~status:`OK ~body:(answer ()) ())))

let () =
  let name, max_number_of_connections =
    match Sys.argv with
    | [|_; ("explicit" | "fitted") as name|] -> name, Some 1000
    | [|_; ("default" | "tiny") as name|] -> name, None
    | _ -> failwith "usage: server (explicit|fitted|default|tiny)"
  in
  Ocsigen.Server.start ?max_number_of_connections
    ~ports:[`Unix ("./" ^ name ^ ".sock"), 0]
    ~logdir:(name ^ "/log") ~uploaddir:None ~command_pipe:(name ^ ".cmd")
    [Ocsigen.Server.host [resources]]
