(* Ocsigen
 * http://www.ocsigen.org
 * Module outputfilter.ml
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
(*****************************************************************************)
(*****************************************************************************)
(* This module allows to rewrite the output sent by the server               *)
(*****************************************************************************)
(*****************************************************************************)

open Lwt
open Ocsigen_extensions
open Simplexmlparser
open Ocsigen_headers

type outputfilter =
  | Rewrite_header of (Http_headers.name * Netstring_pcre.regexp * string)
  | Add_header of (Http_headers.name * string * bool option)

let gen filter = function
  | Req_not_found (code,_) -> return (Ext_next code)
  | Req_found (ri, res) ->
    let new_headers =
      match filter with
      | Rewrite_header (header, regexp, dest) ->
        begin
          try
            let header_values =
              Http_headers.find_all header
                (Ocsigen_http_frame.Result.headers res)
            in
            let h =
              Http_headers.replace_opt header None
                (Ocsigen_http_frame.Result.headers res)
            in
            List.fold_left
              (fun h value ->
                 Http_headers.add
                   header
                   (Netstring_pcre.global_replace regexp dest value)
                   h
              )
              h
              header_values
          with
          | Not_found -> Ocsigen_http_frame.Result.headers res
        end
      | Add_header (header, dest, replace) ->
        begin
          match replace with
          | None ->
            begin
              try
                ignore (Http_headers.find header (Ocsigen_http_frame.Result.headers res));
                (Ocsigen_http_frame.Result.headers res)
              with
              | Not_found ->
                Http_headers.add header dest (Ocsigen_http_frame.Result.headers res)
            end
          | Some false ->
            Http_headers.add header dest (Ocsigen_http_frame.Result.headers res)
          | Some true ->
            Http_headers.replace header dest (Ocsigen_http_frame.Result.headers res)
        end
    in
    Lwt.return
      (Ocsigen_extensions.Ext_found
         (fun () ->
            Lwt.return
              (Ocsigen_http_frame.Result.update res ~headers:new_headers ())))

let gen_code code = function
  | Req_not_found (code,_) -> return (Ext_next code)
  | Req_found (ri, res) ->
    Lwt.return
      (Ocsigen_extensions.Ext_found
         (fun () ->
            Lwt.return (Ocsigen_http_frame.Result.update res ~code ())))



(*****************************************************************************)

let parse_config = function
  (*VVV TODO: rewrite using Ocsigen_extensions.Configuration (see rewritemod) *)
  | Element ("outputfilter", atts, []) ->
    let rec parse_attrs ((h, r, d, rep) as res) = function
      | [] -> res
      | ("header", header)::l when h = None ->
        parse_attrs (Some header, r, d, rep) l
      | ("regexp", regexp)::l when r = None ->
        parse_attrs (h, Some (Netstring_pcre.regexp regexp), d, rep) l
      | ("dest", dest)::l when d = None ->
        parse_attrs (h, r, Some dest, rep) l
      | ("replace", replace)::l when rep = None ->
        let replace =
          try
            bool_of_string replace
          with
          | Invalid_argument _ ->
            raise (Error_in_config_file
                     (Printf.sprintf "Wrong value for attribute replace of <outputfilter/>: %s. is should be true or false" replace))
        in
        parse_attrs (h, r, d, Some replace ) l
      | _ -> raise (Error_in_config_file "Wrong attribute for <outputfilter header=... dest=... (regexp=... / replace=...)/>")
    in
    (match parse_attrs (None, None, None, None) atts with
     | (_, Some _, _, Some _) ->
       raise
         (Error_in_config_file
            "Wrong attributes for <outputfilter/>: attributes regexp and replace can't be set simultaneously")
     | (Some h, Some r, Some d, None) ->
       gen (Rewrite_header (Http_headers.name h, r, d))
     | (Some h, None, Some d, rep) ->
       gen (Add_header (Http_headers.name h, d, rep))
     | _ ->
       raise
         (Error_in_config_file
            "Wrong attributes for <outputfilter header=... dest=... (regexp=... / replace=...)/>"))
  | Element ("sethttpcode", atts, []) ->
    (match atts with
     | [("code", c)] ->
       let code = try int_of_string c
         with Failure _ ->
           raise (Error_in_config_file
                    "invalid code attribute in <sethttpcode>")
       in gen_code code
     | _ ->
       raise (Error_in_config_file
                "Wrong attribute for <sethttpcode code=... />"))
  | Element ("outputfilter", _, _) -> badconfig "Bad syntax for tag <outputfilter header=... dest=... (regexp=... / replace=...)/>"
  | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
  | _ ->
    raise (Error_in_config_file "Unexpected data in config file")





(*****************************************************************************)
(** Registration of the extension *)
let () = register_extension
    ~name:"outputfilter"
    ~fun_site:(fun _ _ _ _ _ -> parse_config)
    ~user_fun_site:(fun _ _ _ _ _ _ -> parse_config)
    ()
