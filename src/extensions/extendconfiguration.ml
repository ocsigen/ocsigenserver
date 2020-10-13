(* Ocsigen
 * http://www.ocsigen.org
 * Module extendconfiguration.ml
 * Copyright (C) 2008 Boris Yakobowski
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

let name = "extendconfiguration"

let bad_config s = raise (Ocsigen_extensions.Error_in_config_file s)

let gen configfun = function
  | Ocsigen_extensions.Req_found _ ->
    Lwt.return Ocsigen_extensions.Ext_do_nothing

  | Ocsigen_extensions.Req_not_found
      (err, ({Ocsigen_extensions.request_config; _} as request)) ->
    Lwt_log.ign_info "Updating configuration";
    let request =
      { request with
        Ocsigen_extensions.request_config =
          configfun request_config }
    in
    Lwt.return
      (Ocsigen_extensions.Ext_continue_with
         (request, Ocsigen_cookie_map.empty, err))

let gather_do_not_serve_files tag =
  let rec aux (regexps, files, extensions) = function
    | [] -> {
        Ocsigen_extensions.do_not_serve_regexps = regexps;
        do_not_serve_files = files;
        do_not_serve_extensions = extensions
      }
    | Xml.Element ("regexp", ["regexp", f], []) :: q ->
      aux (f :: regexps, files, extensions) q
    | Xml.Element ("file", ["file", f], []) :: q ->
      aux (regexps, f :: files, extensions) q
    | Xml.Element ("extension", ["ext", f], []) :: q ->
      aux (regexps, files, f :: extensions) q

    | _ :: _ -> bad_config ("invalid options in tag " ^ tag)
  in
  aux ([], [], [])

exception Bad_regexp of string

let check_regexp_list =
  let hashtbl = Hashtbl.create 17 in
  List.iter @@ fun r ->
  try Hashtbl.find hashtbl r
  with Not_found ->
  try
    ignore (Ocsigen_lib.Netstring_pcre.regexp r);
    Hashtbl.add hashtbl r ()
  with _ -> raise (Bad_regexp r)

let fun_site usermode _ _ _ _ _ = function
  | Xml.Element ("listdirs", ["value", "true"], []) ->
    gen @@ fun config ->
    { config with Ocsigen_extensions.list_directory_content = true }
  | Xml.Element ("listdirs", ["value", "false"], []) ->
    gen @@ fun config ->
    { config with Ocsigen_extensions.list_directory_content = false }
  | Xml.Element ("listdirs" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Xml.Element ("followsymlinks", ["value", follow_symlinks], []) ->
    let follow_symlinks =
      match follow_symlinks with
      | "never" ->
        `No
      | "always" ->
        (match usermode with
         | None ->
           `Always
         | Some _ ->
           raise
             (Ocsigen_extensions.Error_in_user_config_file
                "Cannot specify value 'always' for option \
                 'followsymlinks' in userconf files"))
      | "ownermatch" ->
        `Owner_match
      | _ ->
        bad_config ("Wrong value \"" ^ follow_symlinks ^
                    "\" for option \"followsymlinks\"")
    in
    gen @@ fun config ->
    { config with Ocsigen_extensions.follow_symlinks }
  | Xml.Element ("followsymlinks" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Xml.Element ("charset", attrs, exts) ->
    let rec aux charset_assoc = function
      | [] -> charset_assoc
      | Xml.Element
          ("extension", ["ext", extension; "value", charset], []) :: q ->
        aux
          (Ocsigen_charset_mime.update_charset_ext
             charset_assoc extension charset) q
      | Xml.Element
          ("file", ["file", file; "value", charset], []) :: q ->
        aux
          (Ocsigen_charset_mime.update_charset_file
             charset_assoc file charset)
          q
      | Xml.Element
          ("regexp", ["regexp", regexp; "value", charset], []) :: q ->
        (try
           let r = Ocsigen_lib.Netstring_pcre.regexp regexp in
           aux
             (Ocsigen_charset_mime.update_charset_regexp
                charset_assoc r charset)
             q
         with _ -> bad_config "invalid regexp '%s' in <extension regexp ...>")
      | _ :: _ -> bad_config "invalid subtag in option charset"
    in
    gen (fun config ->
      let config = match attrs with
        | ["default", s] ->
          { config with
            Ocsigen_extensions.charset_assoc =
              Ocsigen_charset_mime.set_default_charset
                config.Ocsigen_extensions.charset_assoc s }
        | [] ->
          config
        | _ ->
          bad_config "Only attribute \"default\" is permitted \
                      for option \"charset\""
      in
      { config with
        Ocsigen_extensions.charset_assoc =
          aux config.Ocsigen_extensions.charset_assoc exts })
  | Xml.Element ("contenttype", attrs, exts) ->
    let rec aux mime_assoc = function
      | [] -> mime_assoc
      | Xml.Element
          ("extension", ["ext", extension; "value", mime], []) :: q ->
        aux
          (Ocsigen_charset_mime.update_mime_ext mime_assoc extension mime)
          q
      | Xml.Element
          ("file", ["file", file; "value", mime], []) :: q ->
        aux (Ocsigen_charset_mime.update_mime_file mime_assoc file mime) q
      | Xml.Element
          ("regexp", ["regexp", regexp; "value", mime], []) :: q ->
        (try
           let r = Ocsigen_lib.Netstring_pcre.regexp regexp in
           aux (Ocsigen_charset_mime.update_mime_regexp mime_assoc r mime) q
         with _ -> bad_config "invalid regexp '%s' in <extension regexp ...>")
      | _ :: _ -> bad_config "invalid subtag in option mime"
    in
    gen (fun config ->
      let config = match attrs with
        | ["default", s] ->
          { config with
            Ocsigen_extensions.mime_assoc =
              Ocsigen_charset_mime.set_default_mime
                config.Ocsigen_extensions.mime_assoc s }
        | [] -> config
        | _ -> bad_config "Only attribute \"default\" is permitted \
                           for option \"contenttype\""
      in
      { config with
        Ocsigen_extensions.mime_assoc =
          aux config.Ocsigen_extensions.mime_assoc exts })
  | Xml.Element ("defaultindex", [], l) ->
    let rec aux indexes = function
      | [] -> List.rev indexes
      | Xml.Element
          ("index", [], [Xml.PCData f]) :: q ->
        aux (f :: indexes) q
      | _ :: _ -> bad_config "subtags must be of the form \
                              <index>...</index> \
                              in option defaultindex"
    in
    gen (fun config ->
      { config with
        Ocsigen_extensions.default_directory_index = aux [] l })
  | Xml.Element ("defaultindex" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Xml.Element ("hidefile", [], l) ->
    let do_not_serve = gather_do_not_serve_files "hidefile" l in
    (try
       check_regexp_list
         do_not_serve.Ocsigen_extensions.do_not_serve_regexps;
       gen (fun config ->
         { config with
           Ocsigen_extensions.do_not_serve_404 =
             Ocsigen_extensions.join_do_not_serve
               do_not_serve
               config.Ocsigen_extensions.do_not_serve_404 })
     with Bad_regexp r ->
       Ocsigen_extensions.badconfig "Invalid regexp %s in %s" r "hidefile")
  | Xml.Element ("hidefile" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Xml.Element ("forbidfile", [], l) ->
    let do_not_serve = gather_do_not_serve_files "forbidfile" l in
    (try
       check_regexp_list
         do_not_serve.Ocsigen_extensions.do_not_serve_regexps;
       gen (fun config ->
         { config with
           Ocsigen_extensions.do_not_serve_403 =
             Ocsigen_extensions.join_do_not_serve
               do_not_serve config.Ocsigen_extensions.do_not_serve_403
         })
     with Bad_regexp r ->
       Ocsigen_extensions.badconfig "Invalid regexp %s in %s" r "forbidfile")
  | Xml.Element ("forbidfile" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Xml.Element
      ("uploaddir", [], [Xml.PCData s]) ->
    gen @@ if s = "" then
      fun config -> { config with Ocsigen_extensions.uploaddir = None }
    else
      fun config -> { config with Ocsigen_extensions.uploaddir = Some s }
  | Xml.Element ("uploaddir" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Xml.Element
      ("maxuploadfilesize" as tag, [], [Xml.PCData s]) ->
    let s =
      try Ocsigen_parseconfig.parse_size_tag "uploaddir" s
      with Ocsigen_config.Config_file_error _ ->
        Ocsigen_extensions.badconfig "Bad syntax for tag %s" tag
    in
    gen @@ fun config ->
    { config with Ocsigen_extensions.maxuploadfilesize = s }
  | Xml.Element ("maxuploadfilesize" as s, _, _) ->
    Ocsigen_extensions.badconfig "Bad syntax for tag %s" s
  | Xml.Element (t, _, _) ->
    raise (Ocsigen_extensions.Bad_config_tag_for_extension t)
  | _ ->
    raise (Ocsigen_extensions.Error_in_config_file
             "Unexpected data in config file")

let () = Ocsigen_extensions.register ~name ~fun_site ()
