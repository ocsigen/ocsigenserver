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

(** Handling Cross-Origin Resource Sharing headers *)

open Ocsigen_lib
open Lwt

module OX      = Ocsigen_extensions
module OMsg    = Ocsigen_messages
module OFrame  = Ocsigen_http_frame
module OStream = Ocsigen_stream
module Http_header = OFrame.Http_header

(*** MAIN FUNCTION ***)

let default_frame () =
  { (OFrame.default_result ()) with
    OFrame.res_code = 200;
    OFrame.res_content_length = Some 0L; }

type config =
    { allowed_method : Http_header.http_method list option;
      (* None means: all method are accepted *)
      allowed_credentials : bool;
      max_age : int option;
      exposed_headers : string list }

let default_config =
  { allowed_method = None;
    allowed_credentials = false;
    max_age = None;
    exposed_headers = [] }

exception Refused

let add_headers config rq response =
  match Lazy.force rq.OX.request_info.OX.ri_origin with
    | None -> return OX.Ext_do_nothing
    | Some origin ->
      OMsg.debug (fun () -> Printf.sprintf "CORS: request with origin: %s" origin);
      let res_headers = response.OFrame.res_headers in

      let res_headers = Http_headers.add
        Http_headers.access_control_allow_origin
        origin
        res_headers
      in

      let res_headers =
        if config.allowed_credentials
        then Http_headers.add
          Http_headers.access_control_allow_credentials
          "true"
          res_headers
        else res_headers
      in

      let res_headers =
        let req_method = Lazy.force
          rq.OX.request_info.OX.ri_access_control_request_method in
        match req_method with
            None -> res_headers
          | Some request_method ->
            let allowed_method =
              match config.allowed_method with
                | None -> true
                | Some l ->
                  try
                    List.mem (Framepp.method_of_string request_method) l
                  with
                    | _ -> false in
            if allowed_method
            then Http_headers.add
              Http_headers.access_control_allow_methods
              request_method res_headers
            else
              (OMsg.debug (fun () -> "CORS: Method refused");
               raise Refused) in

      let res_headers =
        let req_headers = Lazy.force
          rq.OX.request_info.OX.ri_access_control_request_headers in
        match req_headers with
            None -> res_headers
          | Some request_headers ->
            Http_headers.add Http_headers.access_control_allow_headers
              (String.concat ", " request_headers) res_headers in

      let res_headers =
        match config.max_age with
          | None -> res_headers
          | Some max_age ->
            Http_headers.add Http_headers.access_control_max_age
              (string_of_int max_age) res_headers in

      let res_headers =
        match config.exposed_headers with
          | [] -> res_headers
          | _ ->
            Http_headers.add Http_headers.access_control_expose_headers
              (String.concat ", " config.exposed_headers) res_headers in

      return
        (OX.Ext_found (fun () -> return { response with OFrame.res_headers }))


let main config = function

  | OX.Req_not_found (_, rq) ->
      begin match rq.OX.request_info.OX.ri_method with
        | OFrame.Http_header.OPTIONS ->
          OMsg.debug (fun () -> "CORS: OPTIONS request");
          begin
            try
              add_headers config rq (default_frame ())
            with
              | Refused ->
                OMsg.debug (fun () -> "CORS: Refused request");
                Lwt.return OX.Ext_do_nothing
          end
        | _ ->
          Lwt.return OX.Ext_do_nothing
      end

  | OX.Req_found (rq,response) ->
    OMsg.debug (fun () -> "CORS: answered request");
    add_headers config rq response

(*** EPILOGUE ***)

(* registering extension *)

open Simplexmlparser

let comma_space_regexp = Netstring_pcre.regexp "[[:blank:]\n]*,[[:blank:]\n]*"

let parse_attributes config = function
  | ("credentials",b) ->
    { config with allowed_credentials = bool_of_string b }
  | ("max_age",i) ->
    { config with max_age = Some (int_of_string i) }
  | ("exposed_headers",h) ->
    { config with exposed_headers =
        Netstring_pcre.split comma_space_regexp h }
  | ("methods",m) ->
    let l = Netstring_pcre.split comma_space_regexp m in
      { config with allowed_method =
          Some (List.map Framepp.method_of_string l) }
  | (a,_) ->
    OX.badconfig "Unexpected attribute %s for tag cors" a

let parse_config _ _ parse_fun = function
  | Element ("cors", attrs, []) ->
    let config =
      List.fold_left parse_attributes default_config attrs in
    main config
  | Element ("cors", _, _) ->
    OX.badconfig "cors tag should not have children"
  | Element (t, _, _) -> raise (OX.Bad_config_tag_for_extension t)
  | _ ->
    OX.badconfig "Unexpected data in config file"

let site_creator (_ : OX.virtual_hosts) _ = parse_config
let user_site_creator (_ : OX.userconf_info) = site_creator

let () = OX.register_extension
  ~name:"CORS"
  ~fun_site:site_creator
  ~user_fun_site:user_site_creator
  ()
