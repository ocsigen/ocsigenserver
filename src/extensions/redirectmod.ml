(* Ocsigen
 * http://www.ocsigen.org
 * Module redirectmod.ml
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

(* Define page redirections in the configuration file *)

let section = Lwt_log.Section.make "ocsigen:ext:redirectmod"

(* The table of redirections for each virtual server *)
type redirection = {
  r_regexp : Pcre.regexp ;
  r_dest : string ;
  r_full : [`Yes | `No | `Maybe] ;
  r_temp : bool
}

let create_redirection
    ?(full = `Yes)
    ?(temporary = false)
    ~regexp r_dest =
  let r_regexp = Pcre.regexp ("^" ^ regexp ^ "$") in
  { r_regexp ; r_dest ; r_full = full ; r_temp = temporary }

let attempt_redir
    { r_regexp ; r_dest ; r_full ; r_temp }
    _err ri () =
  Lwt_log.ign_info ~section "Is it a redirection?";
  let redir =
    let find full =
      Ocsigen_extensions.find_redirection
        r_regexp full r_dest ri
    in
    match r_full with
    | `Yes ->
      find true
    | `No ->
      find false
    | `Maybe ->
      try
        find false
      with Ocsigen_extensions.Not_concerned ->
        find true
  in
  Lwt_log.ign_info_f ~section
    "YES! %s redirection to: %s"
    (if r_temp then "Temporary " else "Permanent ")
    redir;
  Lwt.return @@ Ocsigen_extensions.Ext_found (fun () ->
    Lwt.return @@
    Ocsigen_response.make @@
    let headers = Cohttp.Header.(init_with "Location" redir)
    and status = if r_temp then `Found else `Moved_permanently in
    Cohttp.Response.make ~status ~headers ())

(** The function that will generate the pages from the request *)
let gen dir = function
  | Ocsigen_extensions.Req_found _ ->
    Lwt.return Ocsigen_extensions.Ext_do_nothing
  | Ocsigen_extensions.Req_not_found
      (err, {Ocsigen_extensions.request_info; _}) ->
    Lwt.catch (attempt_redir dir err request_info) @@ function
    | Ocsigen_extensions.Not_concerned ->
      Lwt.return (Ocsigen_extensions.Ext_next err)
    | e ->
      Lwt.fail e

let parse_config config_elem =
  let regexp = ref None
  and dest = ref ""
  and mode = ref `Yes
  and temporary = ref false in
  Ocsigen_extensions.(
    Configuration.process_element
      ~in_tag:"host"
      ~other_elements:(fun t _ _ -> raise (Bad_config_tag_for_extension t))
      ~elements:[
        Configuration.element
          ~name:"redirect"
          ~attributes:[
            Configuration.attribute
              ~name:"regexp"
              (fun s ->
                 regexp := Some ("^" ^ s ^ "$");
                 mode := `Maybe);
            Configuration.attribute
              ~name:"fullurl"
              (fun s ->
                 regexp := Some s;
                 mode := `Yes);
            Configuration.attribute
              ~name:"suburl"
              (fun s ->
                 regexp := Some s;
                 mode := `No);
            Configuration.attribute
              ~name:"dest"
              ~obligatory:true
              (fun s -> dest := s);
            Configuration.attribute
              ~name:"temporary"
              (function "temporary" -> temporary := true | _ -> ());
          ]
          ()]
      config_elem
  );
  match !regexp with
  | None ->
    Ocsigen_extensions.badconfig "Missing attribute regexp for <redirect>"
  | Some regexp ->
    gen (create_redirection ~full:!mode ~regexp ~temporary:!temporary !dest)

let () =
  Ocsigen_extensions.register
    ~name:"redirectmod"
    ~fun_site:(fun _ _ _ _ _ _ -> parse_config)
    ()

let redirection = Ocsigen_server.Site.Config.key ()

let extension =
  Ocsigen_server.Site.create_extension
    (fun {Ocsigen_server.Site.Config.accessor} ->
       match accessor redirection with
       | Some redirection ->
         gen redirection
       | None ->
         failwith "Redirectmod.redirection not set")
