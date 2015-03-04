(* Ocsigen
 * http://www.ocsigen.org
 * Module userconf.ml
 * Copyright (C) 2007 Vincent Balat
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
(*****************************************************************************)
(*****************************************************************************)
(* Ocsigen module to allow local (users) config files                        *)
(*****************************************************************************)
(*****************************************************************************)


open Lwt
open Ocsigen_lib
open Ocsigen_extensions

exception NoConfFile

let section = Lwt_log.Section.make "ocsigen:ext:userconf"
(*****************************************************************************)

let err_500 =
  Ocsigen_extensions.Ext_stop_site (Ocsigen_cookies.Cookies.empty, 500)


(* Catch invalid userconf files and report an error *)
let handle_parsing_error req = function
  | Ocsigen_extensions.Error_in_config_file s ->
    Lwt_log.ign_error_f ~section
      "Syntax error in userconf configuration file for url %s: %s"
      (Ocsigen_request_info.url_string req.request_info) s;
    Lwt.return err_500

  | Ocsigen_extensions.Error_in_user_config_file s ->
    Lwt_log.ign_error_f ~section
      "Unauthorized option in user configuration for url %s: %s"
      (Ocsigen_request_info.url_string req.request_info) s;
    Lwt.return err_500

  | e -> Lwt.fail e


(* Answer returned by userconf when the url matches *)
let subresult new_req user_parse_site conf previous_err req req_state =
  Ext_sub_result
    (fun awake cookies_to_set rs ->
       (* XXX why is rs above never used ?? *)
       Lwt.catch
         (fun () ->
            user_parse_site conf awake cookies_to_set
              (Ocsigen_extensions.Req_not_found (previous_err, new_req))
            >>= fun (answer, cookies) ->
            (* If the request is not satisfied by userconf, the changes
               in configuration (in request_config) are preserved for the
               remainder of the enclosing <site> (in the Ext_continue
               and Ext_found_continue cases below) *)
            let rec aux ((answer, cts) as r) = match answer with
              | Ext_sub_result sr ->
                (* XXX Are these the good cookies ?? *)
                sr awake cookies_to_set req_state
                >>= aux
              | Ext_continue_with (newreq, cookies, err) ->
                Lwt.return
                  ((Ext_continue_with
                      ({req with request_config = newreq.request_config },
                       cookies, err)), cts)
              | Ext_found_continue_with r ->
                (* We keep config information outside userconf! *)
                Lwt.return
                  (Ext_found_continue_with
                     (fun () ->
                        r () >>= fun (r, newreq) -> Lwt.return
                          (r,
                           { req with request_config = newreq.request_config })
                     ), cts)
              | _ -> Lwt.return r
            in aux (answer, cookies)
         )
         (fun e ->
            handle_parsing_error req e >>=
            fun answer ->
            Lwt.return (answer, Ocsigen_cookies.Cookies.empty))
    )


let conf_to_xml conf =
  try Simplexmlparser.xmlparser_file conf
  with
  | Sys_error _ -> raise NoConfFile
  | Simplexmlparser.Xml_parser_error s ->
    raise (Ocsigen_extensions.Error_in_config_file s)


let gen hostpattern sitepath (regexp, conf, url, prefix, localpath) = function
  | Req_found _ ->
    (* We do not allow setting filters through userconf files right now *)
    Lwt.return Ext_do_nothing

  | Req_not_found (previous_err, req) as req_state->
      let path = (Ocsigen_request_info.sub_path_string req.request_info) in
      match Netstring_pcre.string_match regexp path 0 with
      | None -> Lwt.return (Ext_next previous_err)
      | Some _ ->
          try
            Lwt_log.ign_info ~section "Using user configuration";
            let conf0 = Ocsigen_extensions.replace_user_dir regexp conf path in
            let url = Netstring_pcre.global_replace regexp url path
            and prefix = Netstring_pcre.global_replace regexp prefix path
            and userconf_options = {
              Ocsigen_extensions.localfiles_root =
                Ocsigen_extensions.replace_user_dir regexp localpath path }
            and conf = conf_to_xml conf0
            in
            let user_parse_host = Ocsigen_extensions.parse_user_site_item
              userconf_options hostpattern req.request_config in
            (* Inside userconf, we create a new virtual site starting
               after [prefix], and use a request modified accordingly*)
            let user_parse_site = Ocsigen_extensions.make_parse_config
              (sitepath@[prefix]) user_parse_host
            and path =
              Url.remove_slash_at_beginning
                (Url.remove_dotdot (Neturl.split_path url))
            in
            let new_req =
              { req with request_info =
                  (Ocsigen_request_info.update req.request_info
                   ~sub_path:path
                   ~sub_path_string:url ())}
            in
            Lwt.return
             (subresult new_req user_parse_site conf previous_err req req_state)

      with
      | Ocsigen_extensions.NoSuchUser
      | NoConfFile
      | Unix.Unix_error (Unix.EACCES,_,_)
      | Unix.Unix_error (Unix.ENOENT, _, _) ->
        Lwt.return (Ocsigen_extensions.Ext_next previous_err)
      | e -> handle_parsing_error req e



(*****************************************************************************)
(** Parsing of config file *)

let parse_config hostpattern _ path _ _ config_elem =
  let regexp = ref None in
  let conf = ref None in
  let url = ref None in
  let prefix = ref None in
  let localpath = ref None in
  Ocsigen_extensions.(
    Configuration.process_element
      ~in_tag:"host"
      ~other_elements:(fun t _ _ -> raise (Bad_config_tag_for_extension t))
      ~elements:[
        Configuration.element
          ~name:"userconf"
          ~attributes:[
            Configuration.attribute
              ~name:"regexp"
              ~obligatory:true
              (fun s ->
                 let s = Netstring_pcre.regexp ("^" ^ s ^ "$") in
                 regexp := Some s);
            Configuration.attribute
              ~name:"conf"
              ~obligatory:true
              (fun s ->
                 let s = Ocsigen_extensions.parse_user_dir s in
                 conf := Some s);
            Configuration.attribute
              ~name:"url"
              ~obligatory:true
              (fun s -> url := Some s);
            Configuration.attribute
              ~name:"prefix"
              ~obligatory:true
              (fun s -> prefix := Some s);
            Configuration.attribute
              ~name:"localpath"
              ~obligatory:true
              (fun s ->
                 let s = Ocsigen_extensions.parse_user_dir s in
                 localpath := Some s)
          ]
          ()]
      config_elem
  );
  let info =
    match !regexp, !conf, !url, !prefix, !localpath  with
    | (Some r, Some t, Some u, Some p, Some p') -> (r, t, u, p, p')
    | _ -> badconfig "Missing attributes for <userconf>"
  in
  gen hostpattern path info

(*****************************************************************************)
(** extension registration *)
let () = register_extension
    ~name:"userconf"
    ~fun_site:parse_config
    ()
