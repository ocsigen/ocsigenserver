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

open Printf
open Lwt
open Ocsigen_extensions
open Simplexmlparser
open Ocsigen_http_frame



(*****************************************************************************)
(* Management of basic authentication methods *)

exception Bad_config_tag_for_auth of string

let register_basic_authentication_method, get_basic_authentication_method =
  let fun_auth = ref
      (fun config ->
         raise (Bad_config_tag_for_auth "<unknown basic authentication method>"))
  in

  (********* register_basic_authentication_method *********)
  (fun new_fun_auth ->
     let old_fun_auth = !fun_auth in
     fun_auth :=
       (fun config ->
          try
            old_fun_auth config
          with
          | Bad_config_tag_for_auth c -> new_fun_auth config)),

  (********* get_basic_authentication_method *********)
  (fun config ->
     !fun_auth config)


(*****************************************************************************)
(* Basic authentication with a predefined login/password (example) *)

let _ = register_basic_authentication_method
    (function
      | Element ("plain", ["login", login; "password", password], _) ->
        (fun l p -> Lwt.return (login = l && password = p))
      | _ -> raise (Bad_config_tag_for_extension "not for htpasswd"))


(*****************************************************************************)

let parse_config = function

  | Element ("authbasic", ["realm", realm], auth::[]) ->
    (* http://www.ietf.org/rfc/rfc2617.txt *)
    (* TODO: check that realm is correct *)
    let auth =
      try
        get_basic_authentication_method auth
      with Bad_config_tag_for_extension _ ->
        raise (Error_in_config_file "Unable to find proper authentication method")
    in
    (fun rs ->
       match rs with
       | Ocsigen_extensions.Req_not_found (err, ri) ->
         let reject () =
           let h = Http_headers.add
               (Http_headers.name "WWW-Authenticate")
               (sprintf "Basic realm=\"%s\"" realm)
               Http_headers.empty
           in
           Ocsigen_messages.debug2 "--Access control (auth): invalid credentials!";
           fail (Http_error.Http_exception (401, None, Some h))
         in
         begin try
             let (login, password) =
               let credentials =
                 Http_headers.find
                   (Http_headers.name "Authorization")
                   (Ocsigen_request_info.http_frame ri.request_info)
                   .Ocsigen_http_frame.frame_header
                   .Ocsigen_http_frame.Http_header.headers
               in
               let encoded =
                 let n = String.length credentials in
                 if n > 6 && String.sub credentials 0 6 = "Basic " then
                   String.sub credentials 6 (n-6)
                 else
                   failwith "credentials"
               in
               let decoded = Netencoding.Base64.decode encoded in
               let i = String.index decoded ':' in
               (String.sub decoded 0 i,
                String.sub decoded (i+1) (String.length decoded - (i+1)))
             in
             auth login password >>=
             (fun r ->
                if r then begin
                  Ocsigen_messages.debug2 "--Access control (auth): valid credentials!";
                  Lwt.return (Ocsigen_extensions.Ext_next err)
                end
                else reject ())
           with
           | Not_found -> reject ()
           | e ->
             Ocsigen_messages.debug
               (fun () -> sprintf
                   "--Access control (auth): Invalid Authorization header (%s)"
                   (Printexc.to_string e));
             fail (Ocsigen_http_error (Ocsigen_cookies.Cookies.empty, 400))
         end
       | Ocsigen_extensions.Req_found (ri, r) ->
         Lwt.return Ocsigen_extensions.Ext_do_nothing)

  | Element ("authbasic" as s, _, _) -> badconfig "Bad syntax for tag %s" s

  | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
  | _ -> raise (Error_in_config_file "(authbasic extension) Bad data")




(*****************************************************************************)
(** Registration of the extension *)
let () = register_extension
    ~name:"authbasic"
    ~fun_site:(fun _ _ _ _ _ -> parse_config)
    ~user_fun_site:(fun _ _ _ _ _ _ -> parse_config)
    ()
