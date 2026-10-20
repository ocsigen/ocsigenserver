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
  let allow = of_string "Allow"
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
  let vary = of_string "Vary"
  let user_agent = of_string "User-Agent"
  let referer = of_string "Referer"
  let range = of_string "Range"
  let x_forwarded_for = of_string "X-Forwarded-For"
  let x_forwarded_proto = of_string "X-Forwarded-Proto"

  (* CORS headers *)
  let origin = of_string "Origin"
  let access_control_request_method = of_string "Access-Control-Request-Method"

  let access_control_request_headers =
    of_string "Access-Control-Request-Headers"

  let access_control_allow_origin = of_string "Access-Control-Allow-Origin"

  let access_control_allow_credentials =
    of_string "Access-Control-Allow-Credentials"

  let access_control_expose_headers = of_string "Access-Control-Expose-Headers"
  let access_control_max_age = of_string "Access-Control-Max-Age"
  let access_control_allow_methods = of_string "Access-Control-Allow-Methods"
  let access_control_allow_headers = of_string "Access-Control-Allow-Headers"
end

let connection_options h =
  Cohttp.Header.get_multi h Name.(to_string connection)
  |> List.concat_map (String.split_on_char ',')
  |> List.filter_map (fun option ->
    match String.lowercase_ascii (String.trim option) with
    | "" -> None
    | option -> Some option)

let parse_star a = if a = "*" then None else Some a

let parse_quality f s =
  try
    let a, b = Ocsigen_base.Lib.String.sep ';' s in
    let q, qv = Ocsigen_base.Lib.String.sep '=' b in
    if q = "q" then f a, Some (float_of_string qv) else failwith "Parse error"
  with _ -> f s, None

module Mime_type = struct
  type t = string option * string option

  let parse a =
    let b, c = Ocsigen_base.Lib.String.sep '/' a in
    parse_star b, parse_star c
end

module Accept = struct
  type t = (Mime_type.t * float option * (string * string) list) list

  let parse_extensions parse_name s =
    try
      let a, b = Ocsigen_base.Lib.String.sep ';' s in
      ( parse_name a
      , List.map
          (Ocsigen_base.Lib.String.sep '=')
          (Ocsigen_base.Lib.String.split ';' b) )
    with _ -> parse_name s, []

  let parse_list_with_extensions parse_name s =
    List.map (Ocsigen_base.Lib.String.split ',') s
    |> List.flatten
    |> List.map (parse_extensions parse_name)

  let parse s =
    try
      let l = parse_list_with_extensions Mime_type.parse s in
      let change_quality (a, l) =
        try
          let q, ll = Ocsigen_base.Lib.List.assoc_remove "q" l in
          a, Some (float_of_string q), ll
        with _ -> a, None, l
      in
      List.map change_quality l
    with _ -> []
end

module Accept_encoding = struct
  type t = (string option * float option) list

  let parse s =
    try
      List.map (Ocsigen_base.Lib.String.split ',') s
      |> List.flatten
      |> List.map (parse_quality parse_star)
    with _ -> []
end

module Accept_language = struct
  type t = (string * float option) list

  let parse s =
    try
      List.map (Ocsigen_base.Lib.String.split ',') s
      |> List.flatten
      |> List.map (parse_quality (fun x -> x))
    with _ -> []
end

module Content_type = struct
  let choose accept default alt =
    try
      List.find
        (fun content_type ->
           let f = function
             | (Some a, Some b), _, _ -> a ^ "/" ^ b = content_type
             | _ -> false
           in
           List.exists f accept)
        (default :: alt)
    with Not_found -> default
end

module Entity_tag = struct
  type t = {weak : bool; opaque : string}

  (* The characters of an opaque tag (etagc, RFC 9110 8.8.3): neither a
     space, a control character nor a double quote. *)
  let is_etagc c = c = '!' || (c >= '#' && c <> '\x7f')

  let make ~weak opaque =
    if String.for_all is_etagc opaque
    then {weak; opaque}
    else invalid_arg "Ocsigen_http.Header.Entity_tag.make"

  let to_string {weak; opaque} = (if weak then "W/\"" else "\"") ^ opaque ^ "\""
  let is_space c = c = ' ' || c = '\t'

  (* Parses the comma-separated list of entity-tags of [s] from [pos]. An
     element that is not an entity-tag is skipped up to the next comma. *)
  let parse_tags s =
    let length = String.length s in
    let rec skip_spaces pos =
      if pos < length && is_space s.[pos] then skip_spaces (pos + 1) else pos
    in
    let rec skip_separators pos =
      if pos < length && (is_space s.[pos] || s.[pos] = ',')
      then skip_separators (pos + 1)
      else pos
    in
    let rec skip_element pos =
      if pos < length && s.[pos] <> ',' then skip_element (pos + 1) else pos
    in
    let rec tags acc pos =
      let pos = skip_separators pos in
      if pos >= length
      then List.rev acc
      else
        let weak, quote =
          if pos + 1 < length && s.[pos] = 'W' && s.[pos + 1] = '/'
          then true, pos + 2
          else false, pos
        in
        match
          if quote < length && s.[quote] = '"'
          then String.index_from_opt s (quote + 1) '"'
          else None
        with
        | Some close ->
            let next = skip_spaces (close + 1) in
            let opaque = String.sub s (quote + 1) (close - quote - 1) in
            if next < length && s.[next] <> ','
            then tags acc (skip_element next)
            else if String.for_all is_etagc opaque
            then tags ({weak; opaque} :: acc) next
            else tags acc next
        | None -> tags acc (skip_element pos)
    in
    tags [] 0

  let parse_list values =
    match String.trim (String.concat "," values) with
    | "*" -> `Any
    | s -> `Tags (parse_tags s)

  let weak_equal a b = String.equal a.opaque b.opaque
  let strong_equal a b = (not a.weak) && (not b.weak) && weak_equal a b
end
