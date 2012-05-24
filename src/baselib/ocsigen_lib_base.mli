
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

module Option : sig
  type 'a t = 'a option
  val map : ('a -> 'b) -> 'a t -> 'b t
  val get : (unit -> 'a) -> 'a t -> 'a
  val iter : ('a -> unit) -> 'a t -> unit
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module List : sig
  include module type of List

  val map_filter : ('a -> 'b option) -> 'a list -> 'b list
  val last : 'a list -> 'a
  val assoc_remove : 'a -> ('a * 'b) list -> 'b * ('a * 'b) list

  val remove_first_if_any : 'a -> 'a list -> 'a list
  val remove_first_if_any_q : 'a -> 'a list -> 'a list
  (* val remove_first : 'a -> 'a list -> 'a list *)
  (* val remove_first_q : 'a -> 'a list -> 'a list *)
  (* val remove_all : 'a -> 'a list -> 'a list *)
  (* val remove_all_q : 'a -> 'a list -> 'a list *)
  val remove_all_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
  (* val remove_all_assoc_q : 'a -> ('a * 'b) list -> ('a * 'b) list *)
  (* val is_prefix : 'a list -> 'a list -> bool *)

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

module String_base : sig

  include module type of String

  val remove_spaces : string -> int -> int -> string

  val basic_sep : char -> string -> string * string
  val sep : char -> string -> string * string
  val split : ?multisep:bool -> char -> string -> string list

  val may_append : string -> sep:string -> string -> string (* WAS add_to_string *)
  val may_concat : string -> sep:string -> string -> string (* WAS concat_strings *)

  val first_diff : string -> string -> int -> int -> int

  module Table : Map.S with type key = string
  module Set : Set.S with type elt = string
  module Map : Map.S with type key = string

end

module Url_base : sig

  type t = string
  type uri = string

  val make_absolute_url :
      https:bool -> host:string -> port:int -> uri -> t

  type path = string list

  val remove_dotdot : path -> path
  val remove_end_slash : string -> string
  val remove_internal_slash : path -> path
  val change_empty_list : path -> path
  val add_end_slash_if_missing : path -> path
  val remove_slash_at_end : path -> path
  val remove_slash_at_beginning : path -> path
  (* val recursively_remove_slash_at_beginning : path -> path *)
  val is_prefix_skip_end_slash : string list -> string list -> bool


  val split_fragment : string -> string * string option

end

module Printexc : sig

  include module type of Printexc

  val register_exn_printer : ((exn -> string) -> exn -> string) -> unit

end

val debug : string -> unit
