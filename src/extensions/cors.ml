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

let section = Lwt_log.Section.make "ocsigen:ext:cors"

(*** MAIN FUNCTION ***)

let default_frame () =
  (Ocsigen_http_frame.Result.update (Ocsigen_http_frame.Result.default ())
     ~code:200
     ~content_length:(Some 0L) ())

type config =
  { allowed_method : Ocsigen_http_frame.Http_header.http_method list option;
    (* None means: all method are accepted *)
    allowed_credentials : bool;
    max_age : int option;
    exposed_headers : string list }

exception Refused

let add_headers config rq response =
  match Lazy.force (Ocsigen_request_info
                    .origin rq.Ocsigen_extensions.request_info) with
  | None -> return Ocsigen_extensions.Ext_do_nothing
  | Some origin ->
    Lwt_log.ign_info_f ~section "request with origin: %s" origin;
    let res_headers = (Ocsigen_http_frame.Result.headers response) in

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
          (Ocsigen_request_info
           .access_control_request_method rq.Ocsigen_extensions.request_info)
      in
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
           (Lwt_log.ign_info ~section "Method refused";
           raise Refused) in

    let res_headers =
      let req_headers = Lazy.force
          (Ocsigen_request_info
           .access_control_request_headers rq.Ocsigen_extensions.request_info)
      in
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
      (Ocsigen_extensions.Ext_found (fun () -> return
                                        (Ocsigen_http_frame.Result.update response
                                           ~headers:res_headers ())))

let main config = function

  | Ocsigen_extensions.Req_not_found (_, rq) ->
    begin match (Ocsigen_request_info.meth
                   rq.Ocsigen_extensions.request_info) with
    | Ocsigen_http_frame.Http_header.OPTIONS ->
      Lwt_log.ign_info ~section "OPTIONS request";
      begin
        try
          add_headers config rq (default_frame ())
        with
        | Refused ->
          Lwt_log.ign_info ~section "Refused request";
          Lwt.return Ocsigen_extensions.Ext_do_nothing
      end
    | _ ->
      Lwt.return Ocsigen_extensions.Ext_do_nothing
    end

  | Ocsigen_extensions.Req_found (rq,response) ->
    Lwt_log.ign_info ~section "answered request";
    add_headers config rq response

(*** EPILOGUE ***)

(* registering extension *)

let comma_space_regexp = Netstring_pcre.regexp "[[:blank:]\n]*,[[:blank:]\n]*"

let parse_config _ _ parse_fun config_elem =
  let config = ref
      { allowed_method = None;
        allowed_credentials = false;
        max_age = None;
        exposed_headers = [] }
  in
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
                 config := { !config with allowed_credentials = s });
            Configuration.attribute
              ~name:"max_age"
              (fun s ->
                 let s = Some (int_of_string s) in
                 config := { !config with max_age = s });
            Configuration.attribute
              ~name:"exposed_headers"
              (fun s ->
                 let s = Netstring_pcre.split comma_space_regexp s in
                 config := { !config with exposed_headers = s });
            Configuration.attribute
              ~name:"methods"
              (fun s ->
                 let s = Netstring_pcre.split comma_space_regexp s in
                 let s = Some (List.map Framepp.method_of_string s) in
                 config := { !config with allowed_method = s });
          ]
          ()]
      config_elem
  );
  main !config

let site_creator (_ : Ocsigen_extensions.virtual_hosts) _ = parse_config
let user_site_creator (_ : Ocsigen_extensions.userconf_info) = site_creator

let () = Ocsigen_extensions.register_extension
    ~name:"CORS"
    ~fun_site:site_creator
    ~user_fun_site:user_site_creator
    ()
