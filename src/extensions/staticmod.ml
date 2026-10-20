(* Ocsigen
 * http://www.ocsigen.org
 * Module staticmod.ml
 * Copyright (C) 2005 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Lwt.Infix
module Pcre = Re.Pcre

let name = "staticmod"
let section = Logs.Src.create "ocsigen:ext:staticmod"

(* Structures describing the static pages a each virtual server *)

(* A static site is either an entire directory served unconditionally,
   or a more elaborate redirection based on regexpes and http error
   codes. See the web documentation of staticmod for detail *)
type static_site_kind =
  | Dir of string (* Serves an entire directory *)
  | Regexp of regexp_site

and regexp_site =
  { source_regexp : Pcre.regexp
  ; dest : Ocsigen.Extensions.ud_string
  ; http_status_filter : Pcre.regexp option
  ; root_checks : Ocsigen.Extensions.ud_string option }

(* Finding files *)

(* Does the http status code returned for the page match the given filter ? *)
let http_status_match status_filter status =
  match status_filter with
  | None -> true
  | Some r ->
      Ocsigen_base.Lib.Netstring_pcre.string_match r
        (string_of_int Cohttp.Code.(code_of_status (status :> status_code)))
        0
      <> None

(* Checks that the path specified in a userconf is correct.
   Currently, we check that the path does not contain ".." *)
let correct_user_local_file =
  let regexp = Ocsigen_base.Lib.Netstring_pcre.regexp "(/\\.\\./)|(/\\.\\.$)" in
  fun path ->
    try
      ignore
        (Ocsigen_base.Lib.Netstring_pcre.search_forward regexp path 0
         : int * 'groups);
      false
    with Not_found -> true

(* Is [path] (a list of decoded segments) hidden? It is when one of its
   segments starts with a dot, such as .git/config, .env or .htpasswd: these
   files are seldom meant to be public. The /.well-known/ directory (RFC 8615)
   is the exception. Empty segments are ignored, so that the path of a
   directory listing entry, appended to a path that ends with a slash, is
   checked the same way. *)
let is_hidden path =
  match List.filter (fun s -> s <> "") path with
  | ".well-known" :: path | path ->
      List.exists (String.starts_with ~prefix:".") path

(* Find the local file corresponding to [path] in the static site [dir],
   with [err] as the current http status (in case [dir] is a filter).
   Raises [Ocsigen.Extensions.Not_concerned] if [dir] does not match, or if
   [hidden] is true and [dir] serves the requested path rather than an error
   page. Otherwise returns
   - a boolean indicating that [dir] is an error handler
   - the local file
   If the parameter [usermode] is true, we check that the path
   is valid.
*)
let find_static_page
      ~request
      ~usermode
      ~dir
      ~(err : Cohttp.Code.status)
      ~hidden
      ~pathstring
  =
  let status_filter, filename, root =
    match dir with
    | (Dir _ | Regexp {http_status_filter = None; _}) when hidden ->
        Logs.info ~src:section (fun fmt ->
          fmt "Not serving a path with a segment that starts with a dot");
        raise Ocsigen.Extensions.Not_concerned
    | Dir d -> (
        ( false
        , Filename.concat d pathstring
        , match usermode with
          | None -> Some d
          | Some {Ocsigen.Extensions.localfiles_root} -> Some localfiles_root ))
    | Regexp
        { source_regexp = source
        ; dest
        ; http_status_filter = status_filter
        ; root_checks = rc }
      when http_status_match status_filter err ->
        let status_filter = status_filter <> None
        and file =
          match
            Ocsigen_base.Lib.Netstring_pcre.string_match source pathstring 0
          with
          | None -> raise Ocsigen.Extensions.Not_concerned
          | Some _ -> Ocsigen.Extensions.replace_user_dir source dest pathstring
        and root_checks =
          match rc, usermode with
          | None, Some {Ocsigen.Extensions.localfiles_root = r} -> Some r
          | Some _, Some _ ->
              raise
                (Ocsigen.Extensions.Error_in_user_config_file
                   "Staticmod: cannot specify option 'root' in user configuration files")
          | None, None -> None
          | Some rc, None ->
              Some (Ocsigen.Extensions.replace_user_dir source rc pathstring)
        in
        status_filter, file, root_checks
    | _ -> raise Ocsigen.Extensions.Not_concerned
  in
  if usermode = None || correct_user_local_file filename
  then
    ( status_filter
    , Ocsigen.Local_files.resolve ?no_check_for:root ~request ~filename () )
  else
    raise
      (Ocsigen.Extensions.Error_in_user_config_file
         "Staticmod: cannot use '..' in user paths")

(* Borrowed from TyXML:lib/xml_print.ml (and wrapped) to avoid the dependency *)
let html_of_string s =
  let is_control c =
    let cc = Char.code c in
    cc <= 8 || cc = 11 || cc = 12 || (14 <= cc && cc <= 31) || cc = 127
  in
  let add_unsafe_char b = function
    | '<' -> Buffer.add_string b "&lt;"
    | '>' -> Buffer.add_string b "&gt;"
    | '"' -> Buffer.add_string b "&quot;"
    | '&' -> Buffer.add_string b "&amp;"
    | c when is_control c ->
        Buffer.add_string b "&#";
        Buffer.add_string b (string_of_int (Char.code c));
        Buffer.add_string b ";"
    | c -> Buffer.add_char b c
  in
  let encode_unsafe_char s =
    let b = Buffer.create (String.length s) in
    String.iter (add_unsafe_char b) s;
    Buffer.contents b
  in
  encode_unsafe_char s
(* End of borrowed code *)

(* The listing of directory [dname], requested as [relpath]. The entries for
   which [hidden] is true are left out. *)
let respond_dir ~hidden relpath dname :
  (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
  =
  let readsortdir =
    (* Read a complete directory and sort its entries *)
    let chunk_size = 1024 in
    let rec aux entries dir =
      Lwt_unix.readdir_n dir chunk_size >>= fun chunk ->
      let entries = chunk :: entries in
      if Array.length chunk < chunk_size
      then Lwt.return entries
      else aux entries dir
    in
    Lwt_unix.opendir dname >>= fun dir ->
    Lwt.finalize
      (fun () ->
         aux [] dir >|= fun entries ->
         List.sort compare (List.concat_map Array.to_list entries))
      (fun () -> Lwt_unix.closedir dir)
  in
  Lwt.catch
    (fun () ->
       readsortdir >>= fun entries ->
       let title = html_of_string ("Directory listing for /" ^ relpath) in
       let entries =
         List.filter_map
           (function
             | "." | ".." -> None
             | e when hidden e -> None
             | e ->
                 Some
                   (Printf.sprintf "<li><a href=\"%t\">%t</a></li>"
                      (fun () -> Ocsigen_base.Lib.Url.encode ~plus:false e)
                      (fun () -> html_of_string e)))
           entries
       in
       (* Chunks of [html (head (title x) []) (body [h1 [x]; ul y])] *)
       let chunk1 =
         {|<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml"><head><title>|}
       and chunk2 = {|</title></head><body><h1>|}
       and chunk3 = {|</h1><ul>|}
       and chunkend = {|</ul></body></html>|} in
       let doc =
         chunk1 :: title :: chunk2 :: title :: chunk3 :: (entries @ [chunkend])
       in
       let headers = Cohttp.Header.init_with "content-type" "text/html" in
       Lwt.return
         ( Cohttp.Response.make ~status:`OK ~headers ()
         , Cohttp_lwt.Body.of_string_list doc ))
    (function
      | Unix.Unix_error _ -> Cohttp_lwt_unix.Server.respond_not_found ()
      | exn -> Lwt.fail exn)

let gen ~usermode ~hide_dotfiles ?cache dir = function
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return Ocsigen.Extensions.Ext_do_nothing
  | Ocsigen.Extensions.Req_not_found
      (err, ({Ocsigen.Extensions.request_info; _} as request)) ->
      let try_block () =
        Logs.info ~src:section (fun fmt -> fmt "Is it a static file?");
        let path = Ocsigen.Request.sub_path request_info in
        let pathstring =
          Ocsigen_base.Lib.Url.string_of_url_path ~encode:false path
        in
        let status_filter, page =
          find_static_page ~request ~usermode ~dir ~err
            ~hidden:(hide_dotfiles && is_hidden path)
            ~pathstring
        in
        let serve () =
          (match page with
            | Ocsigen.Local_files.RFile fname ->
                let header name = Ocsigen.Request.header request_info name in
                (* A list header may be written on several lines. *)
                let list_header name =
                  match Ocsigen.Request.header_multi request_info name with
                  | [] -> None
                  | values -> Some (String.concat ", " values)
                in
                Ocsigen.Response.respond_file
                  ?if_none_match:
                    (list_header Ocsigen_http.Header.Name.if_none_match)
                  ?if_modified_since:
                    (header Ocsigen_http.Header.Name.if_modified_since)
                  ?range:(header Ocsigen_http.Header.Name.range)
                  ?if_range:(header Ocsigen_http.Header.Name.if_range)
                  fname
            | Ocsigen.Local_files.RDir dname ->
                let hidden e = hide_dotfiles && is_hidden (path @ [e]) in
                respond_dir ~hidden pathstring dname >>= fun answer ->
                Lwt.return @@ Ocsigen.Response.of_cohttp answer)
          >>= fun answer ->
          let answer =
            if not status_filter
            then answer
            else Ocsigen.Response.set_status answer err
          in
          let answer =
            match cache with
            | None -> answer
            | Some duration ->
                let cache_control, expires =
                  if duration = 0
                  then "no-cache", "0"
                  else
                    ( "max-age=" ^ string_of_int duration
                    , Ocsigen_base.Lib.Date.to_string
                        (Unix.time () +. float_of_int duration) )
                in
                Ocsigen.Response.replace_headers answer
                  [ Ocsigen_http.Header.Name.cache_control, cache_control
                  ; Ocsigen_http.Header.Name.expires, expires ]
          in
          Lwt.return answer
        in
        (* A static page is only read: OPTIONS gets the allowed methods, with
           no content (RFC 9110, section 9.3.7), and other methods 405
           (section 15.5.6). As an error page, it answers any method. *)
        let headers =
          Cohttp.Header.init_with
            Ocsigen_http.Header.Name.(to_string allow)
            "GET, HEAD, OPTIONS"
        in
        (match Ocsigen.Request.meth request_info with
          | `GET | `HEAD -> serve ()
          | _ when status_filter -> serve ()
          | `OPTIONS ->
              Lwt.return
                (Ocsigen.Response.respond_string ~headers ~status:`OK ~body:""
                   ())
          | _ ->
              let headers =
                Cohttp.Header.add headers
                  Ocsigen_http.Header.Name.(to_string content_type)
                  "text/plain; charset=utf-8"
              in
              Lwt.return
                (Ocsigen.Response.respond_string ~headers
                   ~status:`Method_not_allowed ~body:"Method Not Allowed" ()))
        >>= fun answer ->
        Lwt.return (Ocsigen.Extensions.Ext_found (fun () -> Lwt.return answer))
      and catch_block = function
        | Ocsigen.Local_files.Failed_403 ->
            Lwt.return (Ocsigen.Extensions.Ext_next `Forbidden)
        (* XXX We should try to leave an information about this error
         for later *)
        | Ocsigen.Local_files.NotReadableDirectory ->
            Lwt.return (Ocsigen.Extensions.Ext_next err)
        | Ocsigen.Extensions.NoSuchUser | Ocsigen.Extensions.Not_concerned
        | Ocsigen.Local_files.Failed_404 ->
            Lwt.return (Ocsigen.Extensions.Ext_next err)
        | e -> Lwt.fail e
      in
      Lwt.catch try_block catch_block

(*****************************************************************************)
(** Parsing of config file *)

(* In userconf modes, paths must be relative to the root of the userconf config *)
let rewrite_local_path userconf path =
  match userconf with
  | None -> path
  | Some {Ocsigen.Extensions.localfiles_root} -> localfiles_root ^ "/" ^ path

type options =
  { opt_dir : string option
  ; opt_regexp : Pcre.regexp option
  ; opt_code : Pcre.regexp option
  ; opt_dest : Ocsigen.Extensions.ud_string option
  ; opt_root_checks : Ocsigen.Extensions.ud_string option
  ; opt_cache : int option
  ; opt_hide_dotfiles : bool }

let kind dir regexp code dest root_checks =
  match dir, regexp, code, dest, root_checks with
  | None, None, None, _, _ ->
      Ocsigen.Extensions.badconfig
        "Missing attribute dir, regexp, or code for <static>"
  | Some d, None, None, None, None ->
      Dir (Ocsigen_base.Lib.Url.remove_end_slash d)
  | None, Some r, code, Some t, rc ->
      Regexp
        { source_regexp = r
        ; dest = t
        ; http_status_filter = code
        ; root_checks = rc }
  | None, None, (Some _ as code), Some t, None ->
      Regexp
        { dest = t
        ; http_status_filter = code
        ; root_checks = None
        ; source_regexp = Ocsigen_base.Lib.Netstring_pcre.regexp "^.*$" }
  | _ -> Ocsigen.Extensions.badconfig "Wrong attributes for <static>"

let parse_config userconf _ : Ocsigen.Extensions.parse_config_aux =
 fun _ _ _ element ->
  let opt =
    ref
      { opt_dir = None
      ; opt_regexp = None
      ; opt_code = None
      ; opt_dest = None
      ; opt_root_checks = None
      ; opt_cache = None
      ; opt_hide_dotfiles = true }
  in
  Ocsigen.Extensions.(
    Configuration.process_element ~in_tag:"host"
      ~other_elements:(fun t _ _ -> raise (Bad_config_tag_for_extension t))
      ~elements:
        [ Configuration.element ~name:"static"
            ~attributes:
              [ Configuration.attribute ~name:"dir" (fun s ->
                  opt :=
                    {!opt with opt_dir = Some (rewrite_local_path userconf s)})
              ; Configuration.attribute ~name:"regexp" (fun s ->
                  let s =
                    try Ocsigen_base.Lib.Netstring_pcre.regexp ("^" ^ s ^ "$")
                    with Re.Pcre.Parse_error | Re.Pcre.Not_supported ->
                      badconfig "Bad regexp \"%s\" in <static regexp=\"...\" />"
                        s
                  in
                  opt := {!opt with opt_regexp = Some s})
              ; Configuration.attribute ~name:"code" (fun s ->
                  let c =
                    try Ocsigen_base.Lib.Netstring_pcre.regexp ("^" ^ s ^ "$")
                    with Re.Pcre.Parse_error | Re.Pcre.Not_supported ->
                      badconfig "Bad regexp \"%s\" in <static code=\"...\" />" s
                  in
                  opt := {!opt with opt_code = Some c})
              ; Configuration.attribute ~name:"dest" (fun s ->
                  let s =
                    Some (parse_user_dir (rewrite_local_path userconf s))
                  in
                  opt := {!opt with opt_dest = s})
              ; Configuration.attribute ~name:"root" (fun s ->
                  let s = Some (parse_user_dir s) in
                  opt := {!opt with opt_root_checks = s})
              ; Configuration.attribute ~name:"cache" (fun s ->
                  let duration =
                    match s with
                    | "no" -> 0
                    | s -> (
                      try int_of_string s
                      with Failure _ ->
                        badconfig
                          "Bad integer \"%s\" in <static cache=\"...\" />" s)
                  in
                  opt := {!opt with opt_cache = Some duration})
              ; Configuration.attribute ~name:"hidedotfiles" (fun s ->
                  match bool_of_string_opt s with
                  | Some b -> opt := {!opt with opt_hide_dotfiles = b}
                  | None ->
                      badconfig
                        "Bad value \"%s\" in <static hidedotfiles=\"...\" />: it should be true or false"
                        s) ]
            () ]
      element);
  gen ~usermode:userconf ~hide_dotfiles:!opt.opt_hide_dotfiles
    ?cache:!opt.opt_cache
  @@ kind !opt.opt_dir !opt.opt_regexp !opt.opt_code !opt.opt_dest
       !opt.opt_root_checks

let () =
  Ocsigen.Extensions.register ~name
    ~fun_site:(fun path _ -> parse_config path)
    ()

(* TODO: fix names and types, preprocess as we do for XML *)

(* Registration for static linking: *)
let preprocess s = "^" ^ s ^ "$"

let run ?dir ?regexp ?dest ?code ?cache ?root ?(hide_dotfiles = true) () =
  let kind =
    kind dir
      (Option.map (fun x -> Pcre.regexp (preprocess x)) regexp)
      (Option.map (fun x -> Pcre.regexp (preprocess x)) code)
      (Option.map
         (fun x ->
            Ocsigen.Extensions.parse_user_dir (rewrite_local_path None x))
         dest)
      (Option.map
         (fun x ->
            Ocsigen.Extensions.parse_user_dir (rewrite_local_path None x))
         root)
  in
  fun _ _ _ -> gen ~usermode:None ~hide_dotfiles ?cache kind

(* Publish the serving function so that the one-command serve mode can use it
   after loading this extension dynamically, without a static dependency. *)
let () = Ocsigen.Server.register_static_server (fun ~dir -> run ~dir ())
