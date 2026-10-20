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
            if next >= length || s.[next] = ','
            then
              let opaque = String.sub s (quote + 1) (close - quote - 1) in
              tags ({weak; opaque} :: acc) next
            else tags acc (skip_element next)
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

module Date = struct
  let month_of_name = function
    | "Jan" -> Some 1
    | "Feb" -> Some 2
    | "Mar" -> Some 3
    | "Apr" -> Some 4
    | "May" -> Some 5
    | "Jun" -> Some 6
    | "Jul" -> Some 7
    | "Aug" -> Some 8
    | "Sep" -> Some 9
    | "Oct" -> Some 10
    | "Nov" -> Some 11
    | "Dec" -> Some 12
    | _ -> None

  let is_leap year = (year mod 4 = 0 && year mod 100 <> 0) || year mod 400 = 0

  let days_in_month year = function
    | 2 -> if is_leap year then 29 else 28
    | 4 | 6 | 9 | 11 -> 30
    | _ -> 31

  (* Days from 1970-01-01 to the given date of the Gregorian calendar
     (days_from_civil, by Howard Hinnant). *)
  let days_from_civil year month day =
    let year = if month <= 2 then year - 1 else year in
    let era = (if year >= 0 then year else year - 399) / 400 in
    let year_of_era = year - (era * 400) in
    let day_of_year = (((153 * ((month + 9) mod 12)) + 2) / 5) + day - 1 in
    let day_of_era =
      (year_of_era * 365) + (year_of_era / 4) - (year_of_era / 100)
      + day_of_year
    in
    (era * 146097) + day_of_era - 719468

  let seconds ~year ~month ~day ~hour ~minute ~second =
    Float.of_int
      ((days_from_civil year month day * 86400)
      + (hour * 3600) + (minute * 60) + second)

  let make ~year ~month ~day ~hour ~minute ~second =
    if
      day >= 1
      && day <= days_in_month year month
      && hour < 24 && minute < 60 && second <= 60
    then Some (seconds ~year ~month ~day ~hour ~minute ~second)
    else None

  (* A two-digit year is the latest year with these digits that does not put
     the date more than 50 years in the future (RFC 9110 5.6.7). *)
  let full_year ~now ~two_digits ~month ~day ~hour ~minute ~second =
    let current = (Ocsigen_base.Lib.Date.gmtime now).Unix.tm_year + 1900 in
    let base = (current / 100 * 100) + two_digits in
    let limit = now +. (50. *. 365.2425 *. 86400.) in
    let fits year = seconds ~year ~month ~day ~hour ~minute ~second <= limit in
    if fits (base + 100)
    then base + 100
    else if fits base
    then base
    else base - 100

  let is_day_name = function
    | "Mon" | "Tue" | "Wed" | "Thu" | "Fri" | "Sat" | "Sun" -> true
    | _ -> false

  let is_long_day_name = function
    | "Monday" | "Tuesday" | "Wednesday" | "Thursday" | "Friday" | "Saturday"
    | "Sunday" ->
        true
    | _ -> false

  (* [number ~length s] is [s] read as a decimal number of [length] digits,
     or of at most [length] digits when [~up_to] is [true]. *)
  let number ?(up_to = false) ~length s =
    let n = String.length s in
    if
      (n = length || (up_to && n >= 1 && n <= length))
      && String.for_all (function '0' .. '9' -> true | _ -> false) s
    then int_of_string_opt s
    else None

  let parse ?(now = Unix.time ()) s =
    let s = String.trim s in
    let try_format format f =
      try Scanf.sscanf s format f
      with Scanf.Scan_failure _ | Failure _ | End_of_file -> None
    in
    let ( let* ) = Option.bind in
    let ( <|> ) a b = match a with Some _ -> a | None -> b () in
    let time hour minute second k =
      let* hour = number ~length:2 hour in
      let* minute = number ~length:2 minute in
      let* second = number ~length:2 second in
      k ~hour ~minute ~second
    in
    try_format "%3s, %[0-9] %3s %[0-9] %[0-9]:%[0-9]:%[0-9] GMT%!"
      (fun day_name day month year hour minute second ->
         let* () = if is_day_name day_name then Some () else None in
         let* day = number ~length:2 day in
         let* month = month_of_name month in
         let* year = number ~length:4 year in
         time hour minute second (make ~year ~month ~day))
    <|> fun () ->
    try_format "%[A-Za-z], %[0-9]-%3s-%[0-9] %[0-9]:%[0-9]:%[0-9] GMT%!"
      (fun day_name day month year hour minute second ->
         let* () = if is_long_day_name day_name then Some () else None in
         let* day = number ~length:2 day in
         let* month = month_of_name month in
         let* two_digits = number ~length:2 year in
         time hour minute second (fun ~hour ~minute ~second ->
           let year =
             full_year ~now ~two_digits ~month ~day ~hour ~minute ~second
           in
           make ~year ~month ~day ~hour ~minute ~second))
    <|> fun () ->
    try_format "%3s %3s %[0-9] %[0-9]:%[0-9]:%[0-9] %[0-9]%!"
      (fun day_name month day hour minute second year ->
         let* () = if is_day_name day_name then Some () else None in
         let* month = month_of_name month in
         let* day = number ~up_to:true ~length:2 day in
         let* year = number ~length:4 year in
         time hour minute second (make ~year ~month ~day))
end
