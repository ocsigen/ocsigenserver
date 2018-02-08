(* Ocsigen
 * http://www.ocsigen.org
 * Module rewritemod.ml
 * Copyright (C) 2008 Vincent Balat
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

(* Rewrite URLs in the configuration file *)

(* IMPORTANT WARNING

   It is really basic for now:
    - rewrites only subpaths (and doees not change get parameters)
    - changes only ri_sub_path and ri_sub_path_string
   not ri_full_path, nor ri_full_path_string, nor ri_url_string, nor ri_url

   This is probably NOT what we want... *)

let section = Lwt_log.Section.make "ocsigen:ext:rewritemod"

exception Not_concerned

(* The table of rewrites for each virtual server *)
type assockind = Regexp of Pcre.regexp * string * bool

let find_rewrite (Regexp (regexp, dest, fullrewrite)) suburl =
  (match Ocsigen_lib.Netstring_pcre.string_match regexp suburl 0 with
   | None ->
     raise Not_concerned
   | Some _ -> (* Matching regexp found! *)
     Ocsigen_lib.Netstring_pcre.global_replace regexp dest suburl),
  fullrewrite

(* The function that will generate the pages from the request *)
let gen regexp continue = function
  | Ocsigen_extensions.Req_found _ ->
    Lwt.return Ocsigen_extensions.Ext_do_nothing
  | Ocsigen_extensions.Req_not_found (err, ri) ->
    let try_block () =
      Lwt_log.ign_info ~section "Is it a rewrite?";
      let redir, full_rewrite =
        let ri = ri.Ocsigen_extensions.request_info in
        find_rewrite regexp
          (match Ocsigen_request.query ri with
           | None ->
             Ocsigen_request.sub_path_string ri
           | Some g ->
             Ocsigen_request.sub_path_string ri
             ^ "?" ^ g)
      in
      Lwt_log.ign_info_f ~section "YES! rewrite to: %s" redir;
      if continue then
        Lwt.return @@ Ocsigen_extensions.Ext_continue_with
          ({ ri with
             Ocsigen_extensions.request_info =
               Ocsigen_request.update
                 ~full_rewrite
                 ~uri:(Uri.of_string redir)
                 ri.Ocsigen_extensions.request_info
           },
           Ocsigen_cookie_map.empty,
           err)
      else
        Lwt.return @@ Ocsigen_extensions.Ext_retry_with
          ({ ri with
             Ocsigen_extensions.request_info =
               Ocsigen_request.update
                 ~full_rewrite
                 ~uri:(Uri.of_string redir)
                 ri.Ocsigen_extensions.request_info },
           Ocsigen_cookie_map.empty)
    and catch_block = function
      | Ocsigen_extensions.Not_concerned ->
        Lwt.return (Ocsigen_extensions.Ext_next err)
      | e ->
        Lwt.fail e
    in
    Lwt.catch try_block catch_block

let parse_config element =
  let regexp = ref "" in
  let dest = ref None in
  let fullrewrite = ref false in
  let continue = ref false in
  Ocsigen_extensions.(
    Configuration.process_element
      ~in_tag:"host"
      ~other_elements:(fun t _ _ -> raise (Bad_config_tag_for_extension t))
      ~elements:[
        Configuration.element
          ~name:"rewrite"
          ~attributes:[
            Configuration.attribute
              ~name:"regexp"
              ~obligatory:true
              (fun s -> regexp := s);
            Configuration.attribute
              ~name:"url"
              (fun s -> dest := Some s);
            Configuration.attribute
              ~name:"dest"
              (fun s -> dest := Some s);
            Configuration.attribute
              ~name:"fullrewrite"
              (fun s -> fullrewrite := (s = "fullrewrite" || s = "true"));
            Configuration.attribute
              ~name:"continue"
              (fun s -> continue := (s = "continue" || s = "true"));
          ]
          ()]
      element
  );
  match !dest with
  | None ->
    raise
      (Ocsigen_extensions.Error_in_config_file
         "url attribute expected for <rewrite>")
  | Some dest ->
    gen
      (Regexp
         ((Ocsigen_lib.Netstring_pcre.regexp ("^" ^ !regexp ^ "$")),
          dest, !fullrewrite))
      !continue

(** Registration of the extension *)
let () =
  Ocsigen_extensions.register
    ~name:"rewritemod"
    ~fun_site:(fun _ _ _ _ _ _ -> parse_config)
    ()
