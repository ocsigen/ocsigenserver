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

let parse_config config_elem =
  let header = ref None in
  let regexp = ref None in
  let dest = ref None in
  let replace = ref None in
  let code = ref None in
  Ocsigen_extensions.(
    Configuration.process_element
      ~in_tag:"host"
      ~other_elements:(fun t _ _ -> raise (Bad_config_tag_for_extension t))
      ~elements:[
        Configuration.element
          ~name:"outputfilter"
          ~attributes:[
            Configuration.attribute
              ~name:"header"
              (fun s -> header := Some s);
            Configuration.attribute
              ~name:"regexp"
              (fun s -> regexp := Some (Netstring_pcre.regexp s));
            Configuration.attribute
              ~name:"dest"
              (fun s -> dest := Some s);
            Configuration.attribute
              ~name:"replace"
              (fun s ->
                 try replace := Some (bool_of_string s)
                 with
                 | Invalid_argument _ ->
                   badconfig "Wrong value for attribute \
                              replace of <outputfilter/>: \
                              %s. It should be true or false"
                     s
              );
          ] ();
        Configuration.element
          ~name:"sethttpcode"
          ~attributes:[
            Configuration.attribute ~name:"code"
              (fun s ->
                 try code := Some (int_of_string s)
                 with Failure _ ->
                   badconfig "Invalid code attribute in <sethttpcode>"
              );
          ]
          ()]
      config_elem
  );
  match !code with
  | None ->
    begin match !header, !regexp, !dest, !replace with
      | (_, Some _, _, Some _) ->
       badconfig
         "Wrong attributes for <outputfilter/>: attributes regexp and \
          replace can't be set simultaneously"
     | (Some h, Some r, Some d, None) ->
       gen (Rewrite_header (Http_headers.name h, r, d))
     | (Some h, None, Some d, rep) ->
       gen (Add_header (Http_headers.name h, d, rep))
     | _ ->
       badconfig
         "Wrong attributes for <outputfilter header=... dest=... \
          (regexp=... / replace=...)/>"
    end
  | Some code -> gen_code code

(*****************************************************************************)
(** Registration of the extension *)
let () = register_extension
    ~name:"outputfilter"
    ~fun_site:(fun _ _ _ _ _ -> parse_config)
    ~user_fun_site:(fun _ _ _ _ _ _ -> parse_config)
    ()
