(* A server whose first instruction answers each path with a response carrying
   the header or the cookie that the path names. The second one counts the
   open files (../fd-count). *)

open Lwt.Syntax

let with_header name value =
  Ocsigen.Response.respond_string
    ~headers:(Cohttp.Header.of_list [name, value])
    ~status:`OK ~body:"sent\n" ()

let with_cookie_set name cookie =
  Ocsigen.Response.add_cookies
    (Ocsigen.Response.respond_string ~status:`OK ~body:"sent\n" ())
    (Ocsigen_cookie_map.add ~path:[] name cookie Ocsigen_cookie_map.empty)

let with_cookie ?expires ?(secure = false) name value =
  with_cookie_set name (Ocsigen_cookie_map.OSet (expires, value, secure))

let injected = "x\r\nx-injected: 1"

let response = function
  | "tab-in-header" -> Some (with_header "x-a" "a\tb")
  | "crlf-in-header" -> Some (with_header "x-a" injected)
  | "lf-in-header" -> Some (with_header "x-a" "x\nx-injected: 1")
  | "cr-in-header" -> Some (with_header "x-a" "x\rx-injected: 1")
  | "nul-in-header" -> Some (with_header "x-a" "a\000b")
  | "del-in-header" -> Some (with_header "x-a" "a\127b")
  | "non-ascii-in-header" -> Some (with_header "x-a" "\xc3\xa9t\xc3\xa9")
  | "space-in-header-name" -> Some (with_header "x a" "v")
  | "eliom-cookie-name" ->
      Some (with_cookie "eliomdatasession|0|/my site/\xc3\xa9/|" "v")
  | "space-in-cookie-value" -> Some (with_cookie "c" "a b")
  | "secure-cookie" -> Some (with_cookie ~secure:true "c" "v")
  | "secure-cookie-with-expiry" ->
      Some (with_cookie ~expires:1e9 ~secure:true "c" "v")
  | "unset-cookie" -> Some (with_cookie_set "c" Ocsigen_cookie_map.OUnset)
  | "crlf-in-cookie" -> Some (with_cookie "c" injected)
  | "semicolon-in-cookie" -> Some (with_cookie "c" "v; domain=example.org")
  | "equals-in-cookie-name" -> Some (with_cookie "a=b" "v")
  | _ -> None

(* The cookies of an error are written and checked as the others. *)
let error_with_cookie ?(secure = false) value =
  Lwt.fail
    (Ocsigen.Ocsigen_cohttp.Ocsigen_http_error
       ( Ocsigen_cookie_map.add ~path:[] "c"
           (Ocsigen_cookie_map.OSet (None, value, secure))
           Ocsigen_cookie_map.empty
       , `Forbidden
       )
    )

let respond _vh _config_info _path = function
  | Ocsigen.Extensions.Req_not_found (_, {Ocsigen.Extensions.request_info; _})
    -> (
    match Ocsigen.Request.sub_path_string request_info with
    | "file-with-bad-cookie" ->
        (* The body of a file response holds the file open. *)
        let* response = Ocsigen.Response.respond_file "file.txt" in
        let response =
          Ocsigen.Response.add_cookies response
            (Ocsigen_cookie_map.add ~path:[] "c"
               (Ocsigen_cookie_map.OSet (None, injected, false))
               Ocsigen_cookie_map.empty
            )
        in
        Lwt.return (Ocsigen.Extensions.Ext_found (fun () -> Lwt.return response))
    | "error-with-cookie" -> error_with_cookie "v"
    | "error-with-secure-cookie" -> error_with_cookie ~secure:true "v"
    | "error-with-bad-cookie" -> error_with_cookie injected
    | "error-with-bad-header" ->
        (* So are the headers that an extension gives with its error. *)
        Lwt.fail
          (Ocsigen.Ocsigen_cohttp.Ext_http_error
             (`Forbidden, None, Some (Cohttp.Header.of_list ["x-a", injected]))
          )
    | path -> (
      match response path with
      | Some response ->
          Lwt.return
            (Ocsigen.Extensions.Ext_found (fun () -> Lwt.return response))
      | None -> Lwt.return (Ocsigen.Extensions.Ext_next `Not_found)
    )
  )
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return (Ocsigen.Extensions.Ext_next `Not_found)

let () =
  Ocsigen.Server.start
    ~ports:[`Unix "./local.sock", 0]
    ~logdir:"log" ~uploaddir:None ~usedefaulthostname:true
    ~command_pipe:"local.cmd"
    [Ocsigen.Server.host [respond; Fd_count.instruction]]
