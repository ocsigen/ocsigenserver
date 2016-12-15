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

(* Management of basic authentication methods *)

exception Bad_config_tag_for_auth of string

let register_basic_authentication_method, get_basic_authentication_method =

  let fun_auth =
    ref (fun config ->
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
       with Bad_config_tag_for_auth c ->
         new_fun_auth config)),

  (* get_basic_authentication_method *)
  (fun config -> !fun_auth config)

(* Basic authentication with a predefined login/password (example) *)
let _ =
  let open Simplexmlparser in
  register_basic_authentication_method @@ function
  | Element ("plain", ["login", login; "password", password], _) ->
    (fun l p -> Lwt.return (login = l && password = p))
  | _ ->
    raise
      (Ocsigen_extensions.Bad_config_tag_for_extension "not for htpasswd")

let gen ~realm ~auth rs =

  let reject () =
    let h =
      Http_headers.add
        (Http_headers.name "WWW-Authenticate")
        (Printf.sprintf "Basic realm=\"%s\"" realm)
        Http_headers.empty
    in
    Lwt_log.ign_info ~section "AUTH: invalid credentials!";
    Lwt.fail
      (Ocsigen_http_frame.Http_error.Http_exception (401, None, Some h))

  and invalid_header () =
    Lwt_log.ign_info ~section
      "AUTH: invalid Authorization header";
    Lwt.fail
      (Ocsigen_extensions.Ocsigen_http_error
         (Ocsigen_cookies.Cookies.empty, 400))

  in

  let validate ~err s =
    match Cohttp.Auth.credential_of_string s with
    | `Basic (user, pass) ->
      auth user pass >>= fun b ->
      if b then
        Lwt.return (Ocsigen_extensions.Ext_next err)
      else
        reject ()
    | `Other s ->
      invalid_header ()
  in

  match rs with
  | Ocsigen_extensions.Req_not_found (err, ri) ->
    (match
       Ocsigen_cohttp_server.Request.header
         ri.Ocsigen_extensions.request_info
         "Authorization"
     with
     | Some s ->
       validate ~err s
     | None ->
       reject ())
  | Ocsigen_extensions.Req_found (ri, r) ->
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
              Simplexmlparser.Element (name, attrs, content) :: !rest_ref)
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
  Ocsigen_extensions.register_extension
    ~name:"authbasic"
    ~fun_site:(fun _ _ _ _ _ -> parse_config)
    ~user_fun_site:(fun _ _ _ _ _ _ -> parse_config)
    ()
