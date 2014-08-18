(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
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

(** This module just contains only extensions of the standard library and very
    basic Ocsigen values and exceptions. Cf. {!Ocsigen_lib} for functionality
    which depends on specific external libraries.
*)

exception Ocsigen_Internal_Error of string
exception Input_is_too_large
exception Ocsigen_Bad_Request
exception Ocsigen_Request_too_long

val (>>=) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
val (>|=) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t
val (!!) : 'a Lazy.t -> 'a

val (|>) : 'a -> ('a -> 'b) -> 'b
val (@@) : ('a -> 'b) -> 'a -> 'b

external id : 'a -> 'a = "%identity"
val comp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c

module Tuple3 : sig
  val fst : 'a * 'b * 'c -> 'a
  val snd : 'a * 'b * 'c -> 'b
  val thd : 'a * 'b * 'c -> 'c
end

type poly
val to_poly: 'a -> poly
val from_poly: poly -> 'a

type yesnomaybe = Yes | No | Maybe
type ('a, 'b) leftright = Left of 'a | Right of 'b

val advert: string

(** Module Option to compute type ['a option] *)
module Option : sig
  type 'a t = 'a option
  val map : ('a -> 'b) -> 'a t -> 'b t
  val get : (unit -> 'a) -> 'a t -> 'a
  val get' : 'a -> 'a t -> 'a
  val iter : ('a -> unit) -> 'a t -> unit
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val to_list : 'a t -> 'a list
  module Lwt : sig
    val map : ('a -> 'b Lwt.t) -> 'a t -> 'b t Lwt.t
    val get : (unit -> 'a Lwt.t) -> 'a t -> 'a Lwt.t
    val get' : 'a Lwt.t -> 'a t -> 'a Lwt.t
    val iter : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t
    val bind : 'a t -> ('a -> 'b t Lwt.t) -> 'b t Lwt.t
end
end

(** Improvement of module List *)
module List : sig
  include module type of List

  val map_filter : ('a -> 'b option) -> 'a list -> 'b list
  val last : 'a list -> 'a
  val assoc_remove : 'a -> ('a * 'b) list -> 'b * ('a * 'b) list

  val remove_first_if_any : 'a -> 'a list -> 'a list
  val remove_first_if_any_q : 'a -> 'a list -> 'a list
  val remove_first : 'a -> 'a list -> 'a list
  val remove_first_q : 'a -> 'a list -> 'a list
  val remove_all : 'a -> 'a list -> 'a list
  val remove_all_q : 'a -> 'a list -> 'a list
  val remove_all_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
  val remove_all_assoc_q : 'a -> ('a * 'b) list -> ('a * 'b) list
  val is_prefix : 'a list -> 'a list -> bool

  val chop : int -> 'a list -> 'a list

end

(** Circular lists *)
module Clist :
sig
  type 'a t
  type 'a node
  val make : 'a -> 'a node
  val create : unit -> 'a t
  val insert : 'a t -> 'a node -> unit
  val remove : 'a node -> unit
  val value : 'a node -> 'a
  val in_list : 'a node -> bool
  val is_empty : 'a t -> bool

  (** Infinite iteration on circular lists *)
  val iter : ('a -> unit) -> 'a t -> unit

  (** Infinite fold on circular lists (use with care!) *)
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
end

module Int : sig
  module Table : Map.S with type key = int
end

(** Improvement of module String *)
module String_base : sig

  include module type of String

  (** [remove_spaces s beg endd] returns a copy of the string from beg to endd,
      removing spaces at the beginning and at the end *)
  val remove_spaces : string -> int -> int -> string

  (** Cuts a string to the next separator *)
  val basic_sep : char -> string -> string * string

  (** Cuts a string to the next separator, removing spaces.
      Raises [Not_found] if the separator cannot be found. *)
  val sep : char -> string -> string * string

  (** Splits a string for words with separator,
      removing spaces.
      For ex "azert,   sdfmlskdf,    dfdsfs". *)
  val split : ?multisep:bool -> char -> string -> string list

  val may_append : string -> sep:string -> string -> string (* WAS add_to_string *)
  val may_concat : string -> sep:string -> string -> string (* WAS concat_strings *)

  (** [first_diff s1 s2 n last] returns the index of the first difference
     between s1 and s2, starting from n and ending at last.
     returns (last + 1) if no difference is found. *)
  val first_diff : string -> string -> int -> int -> int

  module Table : Map.S with type key = string
  module Set : Set.S with type elt = string
  module Map : Map.S with type key = string

end

module Url_base : sig

  type t = string
  type uri = string

  (** [make_absolute_url https host port path] generates a new absolute url *)
  val make_absolute_url :
    https:bool -> host:string -> port:int -> uri -> t

  type path = string list

  (** [remove_dotdot path] cleans the path of [..] *)
  val remove_dotdot : path -> path

  (** [remove_end_slash str] removes last [/] *)
  val remove_end_slash : string -> string

  (** [remove_internal_slash path] cleans the path of empty string *)
  val remove_internal_slash : path -> path

  val change_empty_list : path -> path
  val add_end_slash_if_missing : path -> path
  val remove_slash_at_end : path -> path
  val remove_slash_at_beginning : path -> path

  (* val recursively_remove_slash_at_beginning : path -> path *)

  (** [is_prefix_skip_end_slash path1 path2] returns [true] if [path1] is the same
      as [path2] before a first slash *)
  val is_prefix_skip_end_slash : string list -> string list -> bool

  (** [split_fragment str] splits [str] at first '#' *)
  val split_fragment : string -> string * string option

end

module Printexc : sig

  include module type of Printexc

  val register_exn_printer : ((exn -> string) -> exn -> string) -> unit

end

val debug : string -> unit
