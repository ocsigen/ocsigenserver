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
   declaration instead of hand-rolled output filters.

   Safe-by-default headers (X-Content-Type-Options, X-Frame-Options,
   Referrer-Policy) are added unless disabled. Strict-Transport-Security and
   Content-Security-Policy are NOT added unless explicitly configured: HSTS is a
   sticky, HTTPS-only commitment (especially with [includeSubDomains]) that the
   server cannot decide safely on its own, and a wrong CSP breaks the
   application. *)

let default_frame_options = "SAMEORIGIN"
let default_referrer_policy = "strict-origin-when-cross-origin"

(* Build the list of (header, value) pairs to add. A header is omitted when its
   option is [None]; [nosniff] adds [X-Content-Type-Options: nosniff]. *)
let headers_of
      ~nosniff
      ~frame_options
      ~referrer_policy
      ~hsts
      ~content_security_policy
  =
  List.filter_map
    (fun x -> x)
    [ (if nosniff then Some ("X-Content-Type-Options", "nosniff") else None)
    ; Option.map (fun v -> "X-Frame-Options", v) frame_options
    ; Option.map (fun v -> "Referrer-Policy", v) referrer_policy
    ; Option.map (fun v -> "Strict-Transport-Security", v) hsts
    ; Option.map (fun v -> "Content-Security-Policy", v) content_security_policy
    ]

(* Add each header to the response unless it is already set, so that a producer
   can override the default. *)
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
  (* Reject CR/LF (and other control characters) in operator-supplied values to
     avoid header injection / response splitting. *)
  let checked name s =
    if String.exists (fun c -> Char.code c < 0x20) s
    then
      Ocsigen.Extensions.badconfig
        "Invalid control character in attribute %s of <securityheaders>" name;
    s
  in
  let nosniff = ref true in
  let frame_options = ref (Some default_frame_options) in
  let referrer_policy = ref (Some default_referrer_policy) in
  let hsts = ref None in
  let content_security_policy = ref None in
  let optional name r s =
    r := if is_disabled s then None else Some (checked name s)
  in
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
              ; Configuration.attribute ~name:"frame-options"
                  (optional "frame-options" frame_options)
              ; Configuration.attribute ~name:"referrer-policy"
                  (optional "referrer-policy" referrer_policy)
              ; Configuration.attribute ~name:"hsts" (optional "hsts" hsts)
              ; Configuration.attribute ~name:"content-security-policy"
                  (optional "content-security-policy" content_security_policy)
              ]
            () ]
      config_elem);
  gen
    (headers_of ~nosniff:!nosniff ~frame_options:!frame_options
       ~referrer_policy:!referrer_policy ~hsts:!hsts
       ~content_security_policy:!content_security_policy)

let () =
  Ocsigen.Extensions.register ~name:"securityheaders"
    ~fun_site:(fun _ _ _ _ _ _ -> parse_config)
    ()

let run
      ?(nosniff = true)
      ?(frame_options = Some default_frame_options)
      ?(referrer_policy = Some default_referrer_policy)
      ?(hsts = None)
      ?content_security_policy
      ()
      _
      _
      _
  =
  gen
    (headers_of ~nosniff ~frame_options ~referrer_policy ~hsts
       ~content_security_policy)
