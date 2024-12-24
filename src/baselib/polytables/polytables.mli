(* Ocsigen
 * Copyright (C) 2009
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
(**
   Polymorphic tables (using Map)
   @author Vincent Balat
   @author Jérôme Vouillon
*)

(** Warning: this module is not thread safe! *)

type 'a key
(** The type of key for a piece of data of type 'a *)

type t
(** The type of tables *)

val create : unit -> t
(** creates a new table *)

val make_key : unit -> 'a key
(** create a new key for each data you want to save *)

val set : table:t -> key:'a key -> value:'a -> unit
(** [set t k v] associates [v] to [k] in [t] *)

val get : table:t -> key:'a key -> 'a
(** [get t k] returns the current binding of [k] in [t] or raises [Not_found] *)

val remove : table:t -> key:'a key -> unit
(** [remove t k] remove the current binding of [k] in [t] if it exists *)

val clear : table:t -> unit
(** [clear t] remove all data from t *)
