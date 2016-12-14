(* Ocsigen
 * http://www.ocsigen.org
 * Module http_headers.mli
 * Copyright (C) 2007 Jérôme Vouillon
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

type name = string
let name : string -> name = String.lowercase
let name_to_string (nm : name) : string = nm

let accept = name "Accept"
let accept_charset = name "Accept-Charset"
let accept_encoding = name "Accept-Encoding"
let accept_language = name "Accept-Language"
let accept_ranges = name "Accept-Ranges"
let authorization = name "Authorization"
let cache_control = name "Cache-Control"
let connection = name "Connection"
let content_disposition = name "Content-Disposition"
let content_encoding = name "Content-Encoding"
let content_range = name "Content-Range"
let content_length = name "Content-Length"
let content_type = name "Content-Type"
let cookie = name "Cookie"
let date = name "Date"
let etag = name "ETag"
let expect = name "Expect"
let expires = name "Expires"
let host = name "Host"
let if_match = name "If-Match"
let if_modified_since = name "If-Modified-Since"
let if_none_match = name "If-None-Match"
let if_unmodified_since = name "If-Unmodified-Since"
let if_range = name "If-Range"
let last_modified = name "Last-Modified"
let location = name "Location"
let pragma = name "Pragma"
let server = name "Server"
let set_cookie = name "Set-Cookie"
let status = name "Status"
let transfer_encoding = name "Transfer-Encoding"
let user_agent = name "User-Agent"
let referer = name "Referer"
let range = name "Range"
let x_forwarded_for = name "X-Forwarded-For"
let x_forwarded_proto = name "X-Forwarded-Proto"

(* CORS headers *)
let origin = name "Origin"
let access_control_request_method = name "Access-Control-Request-Method"
let access_control_request_headers = name "Access-Control-Request-Headers"

let access_control_allow_origin = name "Access-Control-Allow-Origin"
let access_control_allow_credentials = name "Access-Control-Allow-Credentials"
let access_control_expose_headers = name "Access-Control-Expose-Headers"
let access_control_max_age = name "Access-Control-Max-Age"
let access_control_allow_methods = name "Access-Control-Allow-Methods"
let access_control_allow_headers = name "Access-Control-Allow-Headers"

module NameHtbl =
  Hashtbl.Make
    (struct
      type t = name
      let equal n n' = n = n'
      let hash n = Hashtbl.hash n
    end)

(****)

type t = Cohttp.Header.t

let empty = Cohttp.Header.init ()

let find_all name map =
  let l = List.rev (Cohttp.Header.get_multi map name) in
  if l = [] then raise Not_found;
  l

(*XXX We currently return the last header.
  Should we fail if there is more than one? *)

let find name map = match Cohttp.Header.get_multi map name with
  | value :: _ -> value
  | _ -> raise Not_found

let replace name value map = Cohttp.Header.replace map name value
let replace_opt name value map = match value with
  | None -> Cohttp.Header.remove map name
  | Some value -> replace name value map

let add name value map = Cohttp.Header.add map name value

let iter func map =
  Cohttp.Header.iter
    (fun name values -> List.iter (func name) values)
    map

(* XXX:
  * old fold: (name -> string list -> 'a -> 'a) -> t -> 'a -> 'a
  * new fold: (string -> string -> 'a -> 'a) -> t -> 'a -> 'a *)
let fold' func map acc = Cohttp.Header.fold func map acc

let fold func map acc =
  let ( |> ) a f = f a in
  let garbage = Cohttp.Header.fold
      (fun key value garbage ->
         try List.assoc key garbage
             |> fun rest ->
           (key, value :: rest) :: (List.remove_assoc key garbage)
         with Not_found -> (key, [ value ]) :: garbage)
      map []
  in List.fold_left (fun acc (key, values) -> func key values acc) acc garbage

let with_defaults h h' = fold' add h h'

let (<<) h (n, v) = replace n v h

let dyn_headers =
  empty
  << (cache_control, "no-cache")
  << (expires, "0")

type accept =
  ( (string option * string option)
    * float option
    * (string * string) list ) list
