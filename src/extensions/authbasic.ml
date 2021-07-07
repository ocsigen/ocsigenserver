(* Ocsigen
 * http://www.ocsigen.org
 * Module authbasic.ml
 * Copyright (C) 2008 Stéphane Glondu
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

let section = Lwt_log.Section.make "ocsigen:ext:access-control"

type auth = string -> string -> bool Lwt.t

exception Bad_config_tag_for_auth of string

let register_basic_authentication_method, get_basic_authentication_method =

  let fun_auth =
    ref (fun _config ->
      raise
        (Bad_config_tag_for_auth
           "<unknown basic authentication method>"))
  in

  (* register_basic_authentication_method *)
  (fun new_fun_auth ->
     let old_fun_auth = !fun_auth in
     fun_auth := (fun config ->
       try
         old_fun_auth config
       with Bad_config_tag_for_auth _c ->
         new_fun_auth config)),

  (* get_basic_authentication_method *)
  (fun config -> !fun_auth config)

(* Basic authentication with a predefined login/password (example) *)
let _ =
  let open Xml in
  register_basic_authentication_method @@ function
  | Element ("plain", ["login", login; "password", password], _) ->
    (fun l p -> Lwt.return (login = l && password = p))
  | _ ->
    raise
      (Ocsigen_extensions.Bad_config_tag_for_extension "not for htpasswd")

let gen ~realm ~auth rs =

  let reject () =
    let h =
      Cohttp.Header.init_with
        "WWW-Authenticate"
        (Printf.sprintf "Basic realm=\"%s\"" realm)
    in
    Lwt_log.ign_info ~section "AUTH: invalid credentials!";
    Lwt.fail (Ocsigen_cohttp.Ext_http_error
                (`Unauthorized, None, Some h))

  and invalid_header () =
    Lwt_log.ign_info ~section
      "AUTH: invalid Authorization header";
    Lwt.fail
      (Ocsigen_cohttp.Ocsigen_http_error
         (Ocsigen_cookie_map.empty, `Bad_request))

  in

  let validate ~err s =
    match Cohttp.Auth.credential_of_string s with
    | `Basic (user, pass) ->
      auth user pass >>= fun b ->
      if b then
        Lwt.return (Ocsigen_extensions.Ext_next err)
      else
        reject ()
    | `Other _s ->
      invalid_header ()
  in

  match rs with
  | Ocsigen_extensions.Req_not_found (err, ri) ->
    (match
       Ocsigen_request.header
         ri.Ocsigen_extensions.request_info
         Ocsigen_header.Name.authorization
     with
     | Some s ->
       validate ~err s
     | None ->
       reject ())
  | Ocsigen_extensions.Req_found _ ->
    Lwt.return Ocsigen_extensions.Ext_do_nothing

let parse_config element =
  let realm_ref = ref "" in
  let rest_ref = ref [] in
  Ocsigen_extensions.(
    Configuration.process_element
      ~in_tag:"host"
      ~other_elements:(fun t _ _ -> raise (Bad_config_tag_for_extension t))
      ~elements:[
        Configuration.element
          ~name:"authbasic"
          ~attributes:[
            Configuration.attribute
              ~name:"realm"
              ~obligatory:true
              (fun s -> realm_ref := s)
          ]
          ~other_elements:(fun name attrs content ->
            rest_ref :=
              Xml.Element (name, attrs, content) :: !rest_ref)
          ()]
      element
  );
  let realm = !realm_ref in
  let auth = match !rest_ref  with
    | [ x ] -> get_basic_authentication_method x
    | _ -> Ocsigen_extensions.badconfig "Bad syntax for tag authbasic"
  in
  gen ~realm ~auth

(** Registration of the extension *)
let () =
  Ocsigen_extensions.register
    ~name:"authbasic"
    ~fun_site:(fun _ _ _ _ _ _ -> parse_config)
    ()

let realm = Ocsigen_server.Site.Config.key ()

let auth = Ocsigen_server.Site.Config.key ()

let extension =
  Ocsigen_server.Site.create_extension
    (fun {Ocsigen_server.Site.Config.accessor} ->
       match accessor realm, accessor auth with
       | Some realm, Some auth ->
         gen ~realm ~auth
       | _, _ ->
         failwith "Authbasic realm and/or auth not set")
