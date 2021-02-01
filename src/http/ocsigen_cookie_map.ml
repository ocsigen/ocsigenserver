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

module Map_path =
  Map.Make(struct type t = string list let compare = compare end)

module Map_inner = Map.Make(String)

type cookie =
  | OSet of float option * string * bool
  | OUnset

type t = cookie Map_inner.t Map_path.t

let empty = Map_path.empty

let add ~path n v m =
  let m' = try Map_path.find path m with Not_found -> Map_inner.empty in
  (* We replace the old value if it exists *)
  Map_path.add path (Map_inner.add n v m') m

(* [add_multi new old] adds the cookies from [new] to [old]. If
   cookies are already bound in oldcookies, the previous binding
   disappear. *)
let add_multi =
  Map_path.fold @@ fun path ->
  Map_inner.fold @@ fun n v beg ->
  match v with
  | OSet (expo, v, secure) ->
    add ~path n (OSet (expo, v, secure)) beg
  | OUnset ->
    add ~path n OUnset beg

let remove ~path n m =
  try
    let m' = Map_path.find path m in
    let m' = Map_inner.remove n m' in
    if Map_inner.is_empty m' then
      Map_path.remove path m
    else
      (* We replace the old value *)
      Map_path.add path m' m
  with Not_found ->
    m

module Poly = struct
  let add = add
  let remove = remove
end
