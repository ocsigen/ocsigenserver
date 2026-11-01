(* A server whose only instruction answers each path with a response carrying
   the header or the cookie that the path names. *)

let with_header name value =
  Ocsigen.Response.respond_string
    ~headers:(Cohttp.Header.of_list [name, value])
    ~status:`OK ~body:"sent\n" ()

let with_cookie ?(secure = false) name value =
  Ocsigen.Response.add_cookies
    (Ocsigen.Response.respond_string ~status:`OK ~body:"sent\n" ())
    (Ocsigen_cookie_map.add ~path:[] name
       (Ocsigen_cookie_map.OSet (None, value, secure))
       Ocsigen_cookie_map.empty)

let injected = "x\r\nx-injected: 1"

let response = function
  | "tab-in-header" -> Some (with_header "x-a" "a\tb")
  | "crlf-in-header" -> Some (with_header "x-a" injected)
  | "lf-in-header" -> Some (with_header "x-a" "x\nx-injected: 1")
  | "nul-in-header" -> Some (with_header "x-a" "a\000b")
  | "del-in-header" -> Some (with_header "x-a" "a\127b")
  | "non-ascii-in-header" -> Some (with_header "x-a" "\xc3\xa9t\xc3\xa9")
  | "space-in-header-name" -> Some (with_header "x a" "v")
  | "eliom-cookie-name" ->
      Some (with_cookie "eliomdatasession|0|/my site/\xc3\xa9/|" "v")
  | "space-in-cookie-value" -> Some (with_cookie "c" "a b")
  | "secure-cookie" -> Some (with_cookie ~secure:true "c" "v")
  | "crlf-in-cookie" -> Some (with_cookie "c" injected)
  | "semicolon-in-cookie" -> Some (with_cookie "c" "v; domain=example.org")
  | "equals-in-cookie-name" -> Some (with_cookie "a=b" "v")
  | _ -> None

let respond _vh _config_info _path = function
  | Ocsigen.Extensions.Req_not_found (_, {Ocsigen.Extensions.request_info; _})
    -> (
    match Ocsigen.Request.sub_path_string request_info with
    | ("error-with-cookie" | "error-with-bad-cookie") as path ->
        (* The cookies of an error go through the same check. *)
        let value = if path = "error-with-cookie" then "v" else injected in
        Lwt.fail
          (Ocsigen.Ocsigen_cohttp.Ocsigen_http_error
             ( Ocsigen_cookie_map.add ~path:[] "c"
                 (Ocsigen_cookie_map.OSet (None, value, false))
                 Ocsigen_cookie_map.empty
             , `Forbidden ))
    | path -> (
      match response path with
      | Some response ->
          Lwt.return
            (Ocsigen.Extensions.Ext_found (fun () -> Lwt.return response))
      | None -> Lwt.return (Ocsigen.Extensions.Ext_next `Not_found)))
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return (Ocsigen.Extensions.Ext_next `Not_found)

let () =
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ~logdir:"log" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd"
    [Ocsigen.Server.host [respond]]
