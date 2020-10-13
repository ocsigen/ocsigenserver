(* Ocsigen
 * http://www.ocsigen.org
 * Module accesscontrol.ml
 * Copyright (C) 2011 Pierre Chambart
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

(** Handle Cross-Origin Resource Sharing (CORS) headers *)

let section = Lwt_log.Section.make "ocsigen:ext:cors"

(*** MAIN FUNCTION ***)

let default_frame () =
  Ocsigen_response.make (Cohttp.Response.make ~status:`OK ())

type config = {
  methods : Cohttp.Code.meth list option;
  (* None means: all method are accepted *)
  credentials : bool;
  max_age : int option;
  exposed_headers : string list
}

exception Refused

let add_headers config r response =

  match Ocsigen_request.header r Ocsigen_header.Name.origin with

  | None ->
    Lwt.return Ocsigen_extensions.Ext_do_nothing

  | Some origin ->

    Lwt_log.ign_info_f ~section "request with origin: %s" origin;

    let l = [Ocsigen_header.Name.access_control_allow_origin, origin] in

    let l =
      if config.credentials then
        (Ocsigen_header.Name.access_control_allow_credentials, "true") :: l
      else
        l
    in

    let l =
      match
        Ocsigen_request.header r
          Ocsigen_header.Name.access_control_request_method
      with
      | Some request_method ->
        let methods =
          match config.methods with
          | None ->
            true
          | Some l ->
            try
              List.mem (Cohttp.Code.method_of_string request_method) l
            with _ ->
              false
        in
        if methods then
          (Ocsigen_header.Name.access_control_allow_methods,
           request_method)
          :: l
        else
          (Lwt_log.ign_info ~section "Method refused";
           raise Refused)
      | None ->
        l
    in

    let l =
      match
        Ocsigen_request.header r
          Ocsigen_header.Name.access_control_request_headers
      with
      | Some request_headers ->
        (Ocsigen_header.Name.access_control_allow_headers,
         request_headers) :: l
      | None ->
        l
    in

    let l =
      match config.max_age with
      | Some max_age ->
        (Ocsigen_header.Name.access_control_max_age,
         string_of_int max_age) :: l
      | None ->
        l
    in

    let l =
      match config.exposed_headers with
      | [] ->
        l
      | exposed_headers ->
        (Ocsigen_header.Name.access_control_expose_headers,
         String.concat ", " exposed_headers) ::
        l

    in

    Lwt.return
      (Ocsigen_extensions.Ext_found
         (fun () -> Lwt.return @@
           Ocsigen_response.replace_headers response l))

let main config = function

  | Ocsigen_extensions.Req_not_found
      (_, {Ocsigen_extensions.request_info ;_}) ->
    (match Ocsigen_request.meth request_info with
     | `OPTIONS ->
       (Lwt_log.ign_info ~section "OPTIONS request";
        try
          add_headers config request_info (default_frame ())
        with Refused ->
          (Lwt_log.ign_info ~section "Refused request";
           Lwt.return Ocsigen_extensions.Ext_do_nothing))
     | _ ->
       Lwt.return Ocsigen_extensions.Ext_do_nothing)

  | Ocsigen_extensions.Req_found
      ({Ocsigen_extensions.request_info; _}, response) ->
    Lwt_log.ign_info ~section "answered request";
    add_headers config request_info response

(* Register extension *)

let comma_space_regexp =
  Ocsigen_lib.Netstring_pcre.regexp "[[:blank:]\n]*,[[:blank:]\n]*"

let parse_config _ _ _parse_fun config_elem =
  let config = ref {
    methods = None;
    credentials = false;
    max_age = None;
    exposed_headers = []
  } in
  Ocsigen_extensions.(
    Configuration.process_element
      ~in_tag:"host"
      ~other_elements:(fun t _ _ -> raise (Bad_config_tag_for_extension t))
      ~elements:[
        Configuration.element
          ~name:"cors"
          ~attributes:[
            Configuration.attribute
              ~name:"credentials"
              (fun s ->
                 let s = bool_of_string s in
                 config := { !config with credentials = s });
            Configuration.attribute
              ~name:"max_age"
              (fun s ->
                 let s = Some (int_of_string s) in
                 config := { !config with max_age = s });
            Configuration.attribute
              ~name:"exposed_headers"
              (fun s ->
                 let s =
                   Ocsigen_lib.Netstring_pcre.split
                     comma_space_regexp s
                 in
                 config := { !config with exposed_headers = s });
            Configuration.attribute
              ~name:"methods"
              (fun s ->
                 let s =
                   Ocsigen_lib.Netstring_pcre.split
                     comma_space_regexp s
                 in
                 let s = Some (List.map Cohttp.Code.method_of_string s) in
                 config := { !config with methods = s });
          ]
          ()]
      config_elem
  );
  main !config

let () =
  Ocsigen_extensions.register
    ~name:"CORS"
    ~fun_site:(fun _ _ _ -> parse_config)
    ()

let credentials = Ocsigen_server.Site.Config.key ()
let max_age = Ocsigen_server.Site.Config.key ()
let exposed_headers = Ocsigen_server.Site.Config.key ()
let methods = Ocsigen_server.Site.Config.key ()

let extension =
  Ocsigen_server.Site.create_extension
    (fun {Ocsigen_server.Site.Config.accessor} ->
       let methods = accessor methods
       and credentials = Ocsigen_lib.Option.get' false (accessor credentials)
       and max_age = accessor max_age
       and exposed_headers =
         Ocsigen_lib.Option.get' [] (accessor exposed_headers)
       in
       main {credentials ; methods ; max_age ; exposed_headers})
