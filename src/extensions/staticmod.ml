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

let section = Lwt_log.Section.make "ocsigen:ext:staticmod"

exception Not_concerned

(* Structures describing the static pages a each virtual server *)

(* A static site is either an entire directory served unconditionnaly,
   or a more elaborate redirection based on regexpes and http error
   codes. See the web documentation of staticmod for detail *)
type static_site_kind =
  | Dir of string (* Serves an entire directory *)
  | Regexp of regexp_site

and regexp_site = {
  source_regexp: Netstring_pcre.regexp;
  dest: Ocsigen_extensions.ud_string;
  http_status_filter: Netstring_pcre.regexp option;
  root_checks: Ocsigen_extensions.ud_string option;
}

(* Finding files *)

(* Does the http status code returned for the page match the given filter ? *)
let http_status_match status_filter status =
  match status_filter with
  | None -> true
  | Some r ->
    Netstring_pcre.string_match r (string_of_int status) 0 <> None

(* Checks that the path specified in a userconf is correct.
   Currently, we check that the path does not contain ".." *)
let correct_user_local_file =
  let regexp = Netstring_pcre.regexp "(/\\.\\./)|(/\\.\\.$)" in
  fun path ->
    try ignore(Netstring_pcre.search_forward regexp path 0); false
    with Not_found -> true

(* Find the local file corresponding to [path] in the static site [dir],
   with [err] as the current http status (in case [dir] is a filter).
   Raises [Not_Concerned] if [dir] does not match, or returns
   - a boolean indicating that [dir] is an error handler
   - the local file
   If the parameter [usermode] is true, we check that the path
   is valid.
*)
let find_static_page ~request ~usermode ~dir ~err ~pathstring =
  let status_filter, filename, root = match dir with
    | Dir d ->
      (false,
       Filename.concat d pathstring,
       (match usermode with
        | None ->
          Some d
        | Some { Ocsigen_extensions.localfiles_root } ->
          Some localfiles_root
       ))
    | Regexp { source_regexp = source; dest = dest;
               http_status_filter = status_filter;
               root_checks = rc }
      when http_status_match status_filter err ->
      let status_filter = status_filter <> None
      and file =
        match Netstring_pcre.string_match source pathstring 0 with
        | None -> raise Not_concerned
        | Some _ ->
          Ocsigen_extensions.replace_user_dir source dest pathstring
      and root_checks =
        (match rc, usermode with
         | None, Some { Ocsigen_extensions.localfiles_root = r } ->
           Some r
         | Some _, Some _ ->
           raise (Ocsigen_extensions.Error_in_user_config_file
                    "Staticmod: cannot specify option 'root' in \
                     user configuration files")
         | None, None -> None
         | Some rc, None ->
           Some (Ocsigen_extensions.replace_user_dir source rc pathstring)
        )
      in
      status_filter, file, root_checks
    | _ -> raise Not_concerned
  in
  if usermode = None || correct_user_local_file filename then
    (status_filter,
     Ocsigen_local_files.resolve ?no_check_for:root ~request ~filename ())
  else
    raise (Ocsigen_extensions.Error_in_user_config_file
             "Staticmod: cannot use '..' in user paths")


let gen ~usermode ?cache dir = function
  | Ocsigen_extensions.Req_found (_, r) ->
    Lwt.return (Ocsigen_extensions.Ext_do_nothing)
  | Ocsigen_extensions.Req_not_found
      (err, ({ Ocsigen_extensions.request_info } as request)) ->
    let try_block () =
      Lwt_log.ign_info ~section "Is it a static file?";
      let status_filter, page =
        let pathstring =
          Ocsigen_lib.Url.string_of_url_path
            ~encode:false
            (Ocsigen_cohttp_server.Request.path request_info)
        in
        find_static_page ~request ~usermode ~dir ~err ~pathstring
      in
      let fname =
        match page with
        | Ocsigen_local_files.RFile fname ->
          fname
        | Ocsigen_local_files.RDir _ ->
          failwith "FIXME: staticmod dirs not implemented"
      in
      Cohttp_lwt_unix.Server.respond_file ~fname () >>= fun answer ->
      let answer = Ocsigen_cohttp_server.Answer.of_cohttp answer in
      let answer =
        if not status_filter then
          answer
        else
          Ocsigen_cohttp_server.Answer.set_status answer
            (Cohttp.Code.status_of_code err)
      in
      let answer =
        match cache with
        | None ->
          answer
        | Some duration ->
          let cache_control, expires =
            if duration = 0 then
              "no-cache", "0"
            else
              "max-age=" ^ string_of_int duration,
              Ocsigen_http_com.gmtdate
                (Unix.time () +. float_of_int duration)
          in
          Ocsigen_cohttp_server.Answer.replace_headers answer [
            "Cache-Control" , cache_control ;
            "Expires"       , expires       ;
          ]
      in
      Lwt.return (Ocsigen_extensions.Ext_found (fun () -> Lwt.return answer))
    and catch_block = function
      | Ocsigen_local_files.Failed_403 ->
        Lwt.return (Ocsigen_extensions.Ext_next 403)
      (* XXX We should try to leave an information about this error
         for later *)
      | Ocsigen_local_files.NotReadableDirectory ->
        Lwt.return (Ocsigen_extensions.Ext_next err)
      | Ocsigen_extensions.NoSuchUser
      | Ocsigen_extensions.Not_concerned
      | Ocsigen_local_files.Failed_404 ->
        Lwt.return (Ocsigen_extensions.Ext_next err)
      | e ->
        Lwt.fail e
    in
    Lwt.catch try_block catch_block

(*****************************************************************************)
(** Parsing of config file *)

(* In userconf modes, paths must be relative to the root of the userconf config *)
let rewrite_local_path userconf path =
  match userconf with
  | None -> path
  | Some { Ocsigen_extensions.localfiles_root } ->
    localfiles_root ^ "/" ^ path

type options = {
  opt_dir: string option;
  opt_regexp: Netstring_pcre.regexp option;
  opt_code: Netstring_pcre.regexp option;
  opt_dest: Ocsigen_extensions.ud_string option;
  opt_root_checks: Ocsigen_extensions.ud_string option;
  opt_cache: int option;
}

let parse_config userconf _
  : Ocsigen_extensions.parse_config_aux
  = fun _ _ _ element ->
  let opt = ref
    {
      opt_dir = None;
      opt_regexp = None;
      opt_code = None;
      opt_dest = None;
      opt_root_checks = None;
      opt_cache = None;
    }
  in
  Ocsigen_extensions.(
    Configuration.process_element
      ~in_tag:"host"
      ~other_elements:(fun t _ _ -> raise (Bad_config_tag_for_extension t))
      ~elements:[
        Configuration.element
          ~name:"static"
          ~attributes:[
            Configuration.attribute
              ~name:"dir"
              (fun s ->
                 opt := { !opt with opt_dir =
                             Some (rewrite_local_path userconf s) });
            Configuration.attribute
              ~name:"regexp"
              (fun s ->
                 let s =
                   try Netstring_pcre.regexp ("^"^s^"$")
                   with Pcre.Error (Pcre.BadPattern _) ->
                     badconfig
                       "Bad regexp \"%s\" in <static regexp=\"...\" />" s
                 in
                 opt := { !opt with opt_regexp = Some s});
            Configuration.attribute
              ~name:"code"
              (fun s ->
                 let c = try Netstring_pcre.regexp ("^" ^ s ^"$")
                   with Pcre.Error (Pcre.BadPattern _) ->
                     badconfig
                       "Bad regexp \"%s\" in <static code=\"...\" />" s
                 in
                 opt := { !opt with opt_code = Some c });
            Configuration.attribute
              ~name:"dest"
              (fun s ->
                 let s =
                   Some (parse_user_dir (rewrite_local_path userconf s))
                 in
                 opt := { !opt with opt_dest = s });
            Configuration.attribute
              ~name:"root"
              (fun s ->
                 let s = Some (parse_user_dir s) in
                 opt := { !opt with opt_root_checks = s });
            Configuration.attribute
              ~name:"cache"
              (fun s ->
                 let duration = match s with
                   | "no" -> 0
                   | s ->
                     try int_of_string s
                     with Failure _ ->
                       badconfig
                         "Bad integer \"%s\" in <static cache=\"...\" />"
                         s
                 in
                 opt := { !opt with opt_cache = Some duration });
          ]
          ()
      ]
    element
  );
  let kind =
    match !opt.opt_dir,
          !opt.opt_regexp,
          !opt.opt_code,
          !opt.opt_dest,
          !opt.opt_root_checks with
    | (None, None, None, _, _) ->
      Ocsigen_extensions.badconfig
        "Missing attribute dir, regexp, or code for <static>"

    | (Some d, None, None, None, None) ->
      Dir (Ocsigen_lib.Url.remove_end_slash d)

    | (None, Some r, code, Some t, rc) ->
      Regexp { source_regexp = r;
               dest = t;
               http_status_filter = code;
               root_checks = rc;
             }

    | (None, None, (Some _ as code), Some t, None) ->
      Regexp { dest = t; http_status_filter = code; root_checks = None;
               source_regexp = Netstring_pcre.regexp "^.*$" }

    | _ ->
      Ocsigen_extensions.badconfig "Wrong attributes for <static>"
  in
  gen ~usermode:userconf ?cache:!opt.opt_cache kind

(*****************************************************************************)
(** extension registration *)
let () =
  Ocsigen_extensions.register_extension
    ~name:"staticmod"
    ~fun_site:(fun _ -> parse_config None)
    ~user_fun_site:(fun path _ -> parse_config (Some path))
    ()
