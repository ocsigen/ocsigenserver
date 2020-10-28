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

(** This type of maps is used to store cookie values for each
    path. The key has type Url.path option: it is for the path
    (default: root of the site). *)
module Map_path : Map.S with type key := Ocsigen_lib_base.Url_base.path

module Map_inner : Map.S with type key := string

(** Type used for cookies to set. The float option is the timestamp
    for the expiration date. The string is the value. If the bool is
    true and the protocol is https, the cookie will be secure (will ask
    the browser to send it only through secure connections). *)
type cookie =
  | OSet of float option * string * bool
  | OUnset

type t = cookie Map_inner.t Map_path.t

val empty : t

(** [add ~path c v m] adds the cookie [c] to [m].

    If the cookie is already bound, the previous binding disappear. *)
val add :
  path:Ocsigen_lib_base.Url_base.path ->
  string ->
  cookie ->
  t -> t

(** [add_multi new old] adds the cookies from [new] to [old]. If
    cookies are already bound in oldcookies, the previous binding
    disappear. *)
val add_multi : t -> t -> t

(** [remove c cookie_table] removes the cookie [c] from [m].

    Warning: it is not equivalent to [add ... OUnset ...]). *)
val remove :
  path:Ocsigen_lib_base.Url_base.path ->
  string ->
  t -> t

(** Polymorphic versions of [add] and [remove] to use when we don't need to
    OUnset (client-side) *)
module Poly : sig

  val add :
    path:Ocsigen_lib_base.Url_base.path ->
    string ->
    'a ->
    'a Map_inner.t Map_path.t ->
    'a Map_inner.t Map_path.t

  val remove :
    path:Ocsigen_lib_base.Url_base.path ->
    string ->
    'a Map_inner.t Map_path.t ->
    'a Map_inner.t Map_path.t

end
