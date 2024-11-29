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
let section = Lwt_log.Section.make "ocsigen:ext:staticmod"

exception Not_concerned

(* Structures describing the static pages a each virtual server *)

(* A static site is either an entire directory served unconditionally,
   or a more elaborate redirection based on regexpes and http error
   codes. See the web documentation of staticmod for detail *)
type static_site_kind =
  | Dir of string (* Serves an entire directory *)
  | Regexp of regexp_site

and regexp_site =
  { source_regexp : Pcre.regexp
  ; dest : Ocsigen_extensions.ud_string
  ; http_status_filter : Pcre.regexp option
  ; root_checks : Ocsigen_extensions.ud_string option }

(* Finding files *)

(* Does the http status code returned for the page match the given filter ? *)
let http_status_match status_filter status =
  match status_filter with
  | None -> true
  | Some r ->
      Ocsigen_lib.Netstring_pcre.string_match r
        (string_of_int Cohttp.Code.(code_of_status (status :> status_code)))
        0
      <> None

(* Checks that the path specified in a userconf is correct.
   Currently, we check that the path does not contain ".." *)
let correct_user_local_file =
  let regexp = Ocsigen_lib.Netstring_pcre.regexp "(/\\.\\./)|(/\\.\\.$)" in
  fun path ->
    try
      ignore (Ocsigen_lib.Netstring_pcre.search_forward regexp path 0);
      false
    with Not_found -> true

(* Find the local file corresponding to [path] in the static site [dir],
   with [err] as the current http status (in case [dir] is a filter).
   Raises [Not_Concerned] if [dir] does not match, or returns
   - a boolean indicating that [dir] is an error handler
   - the local file
   If the parameter [usermode] is true, we check that the path
   is valid.
*)
let find_static_page ~request ~usermode ~dir ~(err : Cohttp.Code.status)
    ~pathstring
  =
  let status_filter, filename, root =
    match dir with
    | Dir d -> (
        ( false
        , Filename.concat d pathstring
        , match usermode with
          | None -> Some d
          | Some {Ocsigen_extensions.localfiles_root} -> Some localfiles_root ))
    | Regexp
        { source_regexp = source
        ; dest
        ; http_status_filter = status_filter
        ; root_checks = rc }
      when http_status_match status_filter err ->
        let status_filter = status_filter <> None
        and file =
          match Ocsigen_lib.Netstring_pcre.string_match source pathstring 0 with
          | None -> raise Not_concerned
          | Some _ -> Ocsigen_extensions.replace_user_dir source dest pathstring
        and root_checks =
          match rc, usermode with
          | None, Some {Ocsigen_extensions.localfiles_root = r} -> Some r
          | Some _, Some _ ->
              raise
                (Ocsigen_extensions.Error_in_user_config_file
                   "Staticmod: cannot specify option 'root' in user configuration files")
          | None, None -> None
          | Some rc, None ->
              Some (Ocsigen_extensions.replace_user_dir source rc pathstring)
        in
        status_filter, file, root_checks
    | _ -> raise Not_concerned
  in
  if usermode = None || correct_user_local_file filename
  then
    ( status_filter
    , Ocsigen_local_files.resolve ?no_check_for:root ~request ~filename () )
  else
    raise
      (Ocsigen_extensions.Error_in_user_config_file
         "Staticmod: cannot use '..' in user paths")

let respond_dir relpath dname : (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t =
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
       let render e = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) e in
       let t = render (Tyxml.Html.txt ("Directory listing for " ^ relpath)) in
       let entries =
         let open Tyxml.Html in
         List.filter_map
           (function
              | "." | ".." -> None
              | e -> Some (render (li [a ~a:[a_href e] [txt e]])))
           entries
       in
       (* Chunks of [html (head (title t) []) (body [h1 [t]; ul entries])] *)
       let chunk1 =
         {|<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml"><head><title>|}
       and chunk2 = {|</title></head><body><h1>|}
       and chunk3 = {|</h1><ul>|}
       and chunkend = {|</ul></body></html>|} in
       let doc =
         chunk1 :: t :: chunk2 :: t :: chunk3 :: (entries @ [chunkend])
       in
       let headers = Cohttp.Header.init_with "content-type" "text/html" in
       Lwt.return
         ( Cohttp.Response.make ~status:`OK ~headers ()
         , Cohttp_lwt.Body.of_string_list doc ))
    (function
       | Unix.Unix_error _ -> Cohttp_lwt_unix.Server.respond_not_found ()
       | exn -> Lwt.fail exn)

let gen ~usermode ?cache dir = function
  | Ocsigen_extensions.Req_found _ ->
      Lwt.return Ocsigen_extensions.Ext_do_nothing
  | Ocsigen_extensions.Req_not_found
      (err, ({Ocsigen_extensions.request_info; _} as request)) ->
      let try_block () =
        Lwt_log.ign_info ~section "Is it a static file?";
        let pathstring =
          Ocsigen_lib.Url.string_of_url_path ~encode:false
            (Ocsigen_request.sub_path request_info)
        in
        let status_filter, page =
          find_static_page ~request ~usermode ~dir ~err ~pathstring
        in
        (match page with
          | Ocsigen_local_files.RFile fname ->
              Cohttp_lwt_unix.Server.respond_file ~fname ()
          | Ocsigen_local_files.RDir dname -> respond_dir pathstring dname)
        >>= fun answer ->
        let answer = Ocsigen_response.of_cohttp answer in
        let answer =
          if not status_filter
          then answer
          else Ocsigen_response.set_status answer err
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
                  , Ocsigen_lib.Date.to_string
                      (Unix.time () +. float_of_int duration) )
              in
              Ocsigen_response.replace_headers answer
                [ Ocsigen_header.Name.cache_control, cache_control
                ; Ocsigen_header.Name.expires, expires ]
        in
        Lwt.return (Ocsigen_extensions.Ext_found (fun () -> Lwt.return answer))
      and catch_block = function
        | Ocsigen_local_files.Failed_403 ->
            Lwt.return (Ocsigen_extensions.Ext_next `Forbidden)
        (* XXX We should try to leave an information about this error
         for later *)
        | Ocsigen_local_files.NotReadableDirectory ->
            Lwt.return (Ocsigen_extensions.Ext_next err)
        | Ocsigen_extensions.NoSuchUser | Ocsigen_extensions.Not_concerned
        | Ocsigen_local_files.Failed_404 ->
            Lwt.return (Ocsigen_extensions.Ext_next err)
        | e -> Lwt.fail e
      in
      Lwt.catch try_block catch_block

(*****************************************************************************)
(** Parsing of config file *)

(* In userconf modes, paths must be relative to the root of the userconf config *)
let rewrite_local_path userconf path =
  match userconf with
  | None -> path
  | Some {Ocsigen_extensions.localfiles_root} -> localfiles_root ^ "/" ^ path

type options =
  { opt_dir : string option
  ; opt_regexp : Pcre.regexp option
  ; opt_code : Pcre.regexp option
  ; opt_dest : Ocsigen_extensions.ud_string option
  ; opt_root_checks : Ocsigen_extensions.ud_string option
  ; opt_cache : int option }

let kind dir regexp code dest root_checks =
  match dir, regexp, code, dest, root_checks with
  | None, None, None, _, _ ->
      Ocsigen_extensions.badconfig
        "Missing attribute dir, regexp, or code for <static>"
  | Some d, None, None, None, None -> Dir (Ocsigen_lib.Url.remove_end_slash d)
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
        ; source_regexp = Ocsigen_lib.Netstring_pcre.regexp "^.*$" }
  | _ -> Ocsigen_extensions.badconfig "Wrong attributes for <static>"

let parse_config userconf _ : Ocsigen_extensions.parse_config_aux =
 fun _ _ _ element ->
  let opt =
    ref
      { opt_dir = None
      ; opt_regexp = None
      ; opt_code = None
      ; opt_dest = None
      ; opt_root_checks = None
      ; opt_cache = None }
  in
  Ocsigen_extensions.(
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
                    try Ocsigen_lib.Netstring_pcre.regexp ("^" ^ s ^ "$")
                    with Re.Pcre.Parse_error | Re.Pcre.Not_supported ->
                      badconfig "Bad regexp \"%s\" in <static regexp=\"...\" />"
                        s
                  in
                  opt := {!opt with opt_regexp = Some s})
              ; Configuration.attribute ~name:"code" (fun s ->
                  let c =
                    try Ocsigen_lib.Netstring_pcre.regexp ("^" ^ s ^ "$")
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
                  opt := {!opt with opt_cache = Some duration}) ]
            () ]
      element);
  gen ~usermode:userconf ?cache:!opt.opt_cache
  @@ kind !opt.opt_dir !opt.opt_regexp !opt.opt_code !opt.opt_dest
       !opt.opt_root_checks

let () =
  Ocsigen_extensions.register ~name
    ~fun_site:(fun path _ -> parse_config path)
    ()

(* TODO: fix names and types, preprocess as we do for XML *)

(* Registration for static linking: *)
let preprocess s = "^" ^ s ^ "$"

let run ?dir ?regexp ?dest ?code ?cache ?root () =
  let kind =
    kind dir
      (Ocsigen_lib.Option.map (fun x -> Pcre.regexp (preprocess x)) regexp)
      (Ocsigen_lib.Option.map (fun x -> Pcre.regexp (preprocess x)) code)
      (Ocsigen_lib.Option.map
         (fun x ->
            Ocsigen_extensions.parse_user_dir (rewrite_local_path None x))
         dest)
      (Ocsigen_lib.Option.map
         (fun x ->
            Ocsigen_extensions.parse_user_dir (rewrite_local_path None x))
         root)
  in
  fun _ _ _ -> gen ~usermode:None ?cache kind
