(* Ocsigen
 * Copyright (C) 2010 Vincent Balat
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

open Ocsigen_lib

module CookiesTable : Map.S with type key = string

(** This table is to store cookie values for each path.
    The key has type Url.path option:
    it is for the path (default: root of the site),
*)
module Cookies : Map.S with type key = Url.path

(** Type used for cookies to set.
    The float option is the timestamp for the expiration date.
    The string is the value.
    If the bool is true and the protocol is https, the cookie will be secure
    (will ask the browser to send it only through secure connections).
*)
type cookie =
  | OSet of float option * string * bool
  | OUnset

type cookieset = cookie CookiesTable.t Cookies.t

val empty_cookieset : 'a CookiesTable.t Cookies.t


(** [add_cookie path c v cookie_table]
    adds the cookie [c] to the table [cookie_table].
    If the cookie is already bound, the previous binding disappear. *)
val add_cookie : Url.path -> string -> 'a ->
  'a CookiesTable.t Cookies.t ->
  'a CookiesTable.t Cookies.t

(** [remove_cookie c cookie_table] removes the cookie [c]
    from the table [cookie_table].
    Warning: it is not equivalent to [add_cookie ... OUnset ...]).
*)
val remove_cookie : Url.path -> string ->
  'a CookiesTable.t Cookies.t ->
  'a CookiesTable.t Cookies.t

(** [add_cookies newcookies oldcookies] adds the cookies from [newcookies]
    to [oldcookies]. If cookies are already bound in oldcookies,
    the previous binding disappear. *)
val add_cookies :
  cookie CookiesTable.t Cookies.t ->
  cookie CookiesTable.t Cookies.t ->
  cookie CookiesTable.t Cookies.t
