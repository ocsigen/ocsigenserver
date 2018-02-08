(* Ocsigen
 * http://www.ocsigen.org
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

type t = Cohttp.Header.t

let of_option = function Some h -> h | None -> Cohttp.Header.init ()

module Name = struct

  type t = string

  let of_string = String.lowercase_ascii
  let to_string s = s

  let accept = of_string "Accept"
  let accept_charset = of_string "Accept-Charset"
  let accept_encoding = of_string "Accept-Encoding"
  let accept_language = of_string "Accept-Language"
  let accept_ranges = of_string "Accept-Ranges"
  let authorization = of_string "Authorization"
  let cache_control = of_string "Cache-Control"
  let connection = of_string "Connection"
  let content_disposition = of_string "Content-Disposition"
  let content_encoding = of_string "Content-Encoding"
  let content_range = of_string "Content-Range"
  let content_length = of_string "Content-Length"
  let content_type = of_string "Content-Type"
  let cookie = of_string "Cookie"
  let date = of_string "Date"
  let etag = of_string "ETag"
  let expect = of_string "Expect"
  let expires = of_string "Expires"
  let host = of_string "Host"
  let if_match = of_string "If-Match"
  let if_modified_since = of_string "If-Modified-Since"
  let if_none_match = of_string "If-None-Match"
  let if_unmodified_since = of_string "If-Unmodified-Since"
  let if_range = of_string "If-Range"
  let last_modified = of_string "Last-Modified"
  let location = of_string "Location"
  let pragma = of_string "Pragma"
  let server = of_string "Server"
  let set_cookie = of_string "Set-Cookie"
  let status = of_string "Status"
  let transfer_encoding = of_string "Transfer-Encoding"
  let user_agent = of_string "User-Agent"
  let referer = of_string "Referer"
  let range = of_string "Range"
  let x_forwarded_for = of_string "X-Forwarded-For"
  let x_forwarded_proto = of_string "X-Forwarded-Proto"

  (* CORS headers *)
  let origin = of_string "Origin"
  let access_control_request_method =
    of_string "Access-Control-Request-Method"
  let access_control_request_headers =
    of_string "Access-Control-Request-Headers"
  let access_control_allow_origin =
    of_string "Access-Control-Allow-Origin"
  let access_control_allow_credentials =
    of_string "Access-Control-Allow-Credentials"
  let access_control_expose_headers =
    of_string "Access-Control-Expose-Headers"
  let access_control_max_age =
    of_string "Access-Control-Max-Age"
  let access_control_allow_methods =
    of_string "Access-Control-Allow-Methods"
  let access_control_allow_headers =
    of_string "Access-Control-Allow-Headers"

end

let parse_star a =
  if a = "*" then
    None
  else
    Some a

let parse_quality f s =
  try
    let a, b = Ocsigen_lib.String.sep ';' s in
    let q, qv = Ocsigen_lib.String.sep '=' b in
    if q = "q" then
      f a, Some (float_of_string qv)
    else
      failwith "Parse error"
  with _ ->
    f s, None

module Mime_type = struct

  type t = string option * string option

  let parse a =
    let b, c = Ocsigen_lib.String.sep '/' a in
    parse_star b, parse_star c

end

module Accept = struct

  type t =
    (Mime_type.t *
     float option *
     (string * string) list) list

  let parse_extensions parse_name s =
    try
      let a, b = Ocsigen_lib.String.sep ';' s in
      parse_name a,
      List.map
        (Ocsigen_lib.String.sep '=')
        (Ocsigen_lib.String.split ';' b)
    with _ ->
      parse_name s, []

  let parse_list_with_extensions parse_name s =
    List.map (Ocsigen_lib.String.split ',') s
    |> List.flatten
    |> List.map (parse_extensions parse_name)

  let parse s =
    try
      let l = parse_list_with_extensions Mime_type.parse s in
      let change_quality (a, l) =
        try
          let q, ll = Ocsigen_lib.List.assoc_remove "q" l in
          a, Some (float_of_string q), ll
        with _ ->
          a, None, l
      in
      List.map change_quality l
    with _ -> []

end

module Accept_encoding = struct

  type t = (string option * float option) list

  let parse s =
    try
      List.map (Ocsigen_lib.String.split ',') s
      |> List.flatten
      |> List.map (parse_quality parse_star)
    with _ ->
      []

end

module Accept_language = struct

  type t = (string * float option) list

  let parse s =
    try
      List.map (Ocsigen_lib.String.split ',') s
      |> List.flatten
      |> List.map (parse_quality (fun x -> x))
    with _ ->
      []

end

module Content_type = struct

  let choose accept default alt =
    try
      List.find
        (fun content_type ->
           let f = function
             | (Some a, Some b), _, _ ->
               a ^ "/" ^ b = content_type
             | _ ->
               false
           in
           List.exists f accept)
        (default :: alt)
    with Not_found ->
      default

end
