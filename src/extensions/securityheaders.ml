(* Ocsigen
 * http://www.ocsigen.org
 * Module securityheaders.ml
 * Copyright (C) 2026 Vincent Balat
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

(* This module adds common security-related headers to responses, with one
   declaration instead of hand-rolled output filters. *)

let default_hsts = "max-age=15552000; includeSubDomains"
let default_frame_options = "SAMEORIGIN"

(* Build the list of (header, value) pairs to add. A header is omitted when its
   option is [None]; [nosniff] adds [X-Content-Type-Options: nosniff]. *)
let headers_of ~nosniff ~frame_options ~hsts ~content_security_policy =
  List.filter_map
    (fun x -> x)
    [ (if nosniff then Some ("X-Content-Type-Options", "nosniff") else None)
    ; Option.map (fun v -> "X-Frame-Options", v) frame_options
    ; Option.map (fun v -> "Strict-Transport-Security", v) hsts
    ; Option.map (fun v -> "Content-Security-Policy", v) content_security_policy
    ]

(* Add each header to the response unless it is already set. *)
let gen headers = function
  | Ocsigen.Extensions.Req_not_found (code, _) ->
      Lwt.return (Ocsigen.Extensions.Ext_next code)
  | Ocsigen.Extensions.Req_found (_ri, res) ->
      Lwt.return
      @@ Ocsigen.Extensions.Ext_found
           (fun () ->
             Lwt.return
             @@ List.fold_left
                  (fun res (name, value) ->
                     let name = Ocsigen_http.Header.Name.of_string name in
                     match Ocsigen.Response.header res name with
                     | Some _ -> res
                     | None -> Ocsigen.Response.add_header res name value)
                  res headers)

(* ["no"], ["none"] and the empty string disable a header. *)
let is_disabled s =
  match String.lowercase_ascii (String.trim s) with
  | "no" | "none" | "" -> true
  | _ -> false

let parse_config config_elem =
  let nosniff = ref true in
  let frame_options = ref (Some default_frame_options) in
  let hsts = ref (Some default_hsts) in
  let content_security_policy = ref None in
  Ocsigen.Extensions.(
    Configuration.process_element ~in_tag:"host"
      ~other_elements:(fun t _ _ -> raise (Bad_config_tag_for_extension t))
      ~elements:
        [ Configuration.element ~name:"securityheaders"
            ~attributes:
              [ Configuration.attribute ~name:"nosniff" (fun s ->
                  nosniff :=
                    not
                      (match String.lowercase_ascii (String.trim s) with
                      | "false" | "no" | "off" -> true
                      | _ -> false))
              ; Configuration.attribute ~name:"frame-options" (fun s ->
                  frame_options := if is_disabled s then None else Some s)
              ; Configuration.attribute ~name:"hsts" (fun s ->
                  hsts := if is_disabled s then None else Some s)
              ; Configuration.attribute ~name:"content-security-policy"
                  (fun s -> content_security_policy := Some s) ]
            () ]
      config_elem);
  gen
    (headers_of ~nosniff:!nosniff ~frame_options:!frame_options ~hsts:!hsts
       ~content_security_policy:!content_security_policy)

let () =
  Ocsigen.Extensions.register ~name:"securityheaders"
    ~fun_site:(fun _ _ _ _ _ _ -> parse_config)
    ()

let run
      ?(nosniff = true)
      ?(frame_options = Some default_frame_options)
      ?(hsts = Some default_hsts)
      ?content_security_policy
      ()
      _
      _
      _
  =
  gen (headers_of ~nosniff ~frame_options ~hsts ~content_security_policy)
