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

(* This module enables rewritting the server output *)

type header_filter = [
  | `Rewrite of (Ocsigen_header.Name.t * Pcre.regexp * string)
  | `Add of (Ocsigen_header.Name.t * string * bool option)
]

let gen filter = function
  | Ocsigen_extensions.Req_not_found (code, _) ->
    Lwt.return (Ocsigen_extensions.Ext_next code)
  | Ocsigen_extensions.Req_found (_ri, res) ->
    Lwt.return @@ Ocsigen_extensions.Ext_found (fun () ->
      Lwt.return @@ match filter with
      | `Rewrite (header, regexp, dest) ->
        (try
           let l =
             List.map
               (Ocsigen_lib.Netstring_pcre.global_replace regexp dest)
               (Ocsigen_response.header_multi res header)
           and a = Ocsigen_response.remove_header res header in
           Ocsigen_response.add_header_multi a header l
         with Not_found ->
           res)
      | `Add (header, dest, replace) ->
        match replace with
        | None ->
          (match Ocsigen_response.header res header with
           | Some _ ->
             res
           | None ->
             Ocsigen_response.add_header res header dest)
        | Some false ->
          Ocsigen_response.add_header res header dest
        | Some true ->
          Ocsigen_response.replace_header res header dest)

let gen_code code = function
  | Ocsigen_extensions.Req_not_found (code, _) ->
    Lwt.return (Ocsigen_extensions.Ext_next code)
  | Ocsigen_extensions.Req_found (_ri, res) ->
    Lwt.return @@ Ocsigen_extensions.Ext_found (fun () ->
      Lwt.return (Ocsigen_response.set_status res code))

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
              (fun s ->
                 regexp :=
                   Some (Ocsigen_lib.Netstring_pcre.regexp s));
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
                 try
                   match
                     Cohttp.Code.status_of_code (int_of_string s)
                   with
                   | #Cohttp.Code.status as status ->
                     code := Some status
                   | `Code _ ->
                     failwith "Invalid code"
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
        Ocsigen_extensions.badconfig
          "Wrong attributes for <outputfilter/>: attributes regexp and \
           replace can't be set simultaneously"
      | (Some h, Some r, Some d, None) ->
        gen (`Rewrite (Ocsigen_header.Name.of_string h, r, d))
      | (Some h, None, Some d, rep) ->
        gen (`Add (Ocsigen_header.Name.of_string h, d, rep))
      | _ ->
        Ocsigen_extensions.badconfig
          "Wrong attributes for <outputfilter header=... dest=... \
           (regexp=... / replace=...)/>"
    end
  | Some code -> gen_code code

let () =
  Ocsigen_extensions.register
    ~name:"outputfilter"
    ~fun_site:(fun _ _ _ _ _ _ -> parse_config)
    ()

let mode = Ocsigen_server.Site.Config.key ()

let extension =
  Ocsigen_server.Site.create_extension
    (fun {Ocsigen_server.Site.Config.accessor} ->
       match accessor mode with
       | Some (`Code c)  ->
         gen_code c
       | Some (#header_filter as f) ->
         gen f
       | None ->
         failwith "Outputfilter.mode not set")
