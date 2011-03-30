
exception Ocsigen_Internal_Error of string
exception Input_is_too_large
exception Ocsigen_Bad_Request
exception Ocsigen_Request_too_long

external id : 'a -> 'a = "%identity"

val (>>=) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
val (>|=) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t

val comp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
(* val uncurry2 : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c *)

(* val map_option : ('a -> 'b) -> 'a option -> 'b option *)

(* val fst3 : 'a * 'b * 'c -> 'a *)
(* val snd3 : 'a * 'b * 'c -> 'b *)
(* val thd3 : 'a * 'b * 'c -> 'c *)

type yesnomaybe = Yes | No | Maybe
type ('a, 'b) leftright = Left of 'a | Right of 'b

val advert: string


module List : sig
  include module type of List

  (* val remove_first_if_any : 'a -> 'a list -> 'a list *)
  (* val remove_first_if_any_q : 'a -> 'a list -> 'a list *)
  (* val remove_first : 'a -> 'a list -> 'a list *)
  (* val remove_first_q : 'a -> 'a list -> 'a list *)
  (* val remove_all : 'a -> 'a list -> 'a list *)
  (* val remove_all_q : 'a -> 'a list -> 'a list *)
  (* val remove_all_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list *)
  (* val remove_all_assoc_q : 'a -> ('a * 'b) list -> ('a * 'b) list *)
  val last : 'a list -> 'a
  val assoc_remove : 'a -> ('a * 'b) list -> 'b * ('a * 'b) list
  (* val is_prefix : 'a list -> 'a list -> bool *)

end

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
  val iter : ('a -> unit) -> 'a t -> unit
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
end

(*
module Int : sig
  module Table : Map.S with type key = int
end
*)

module String : sig
  include module type of String

  val remove_spaces : string -> int -> int -> string

  (* val basic_sep : char -> string -> string * string *)
  val sep : char -> string -> string * string
  val split : ?multisep:bool -> char -> string -> string list

  (* val may_append : string -> sep:string -> string -> string (\* WAS add_to_string *\) *)
  (* val may_concat : string -> sep:string -> string -> string (\* WAS concat_strings *\) *)

  (* val first_diff : string -> string -> int -> int -> int *)

  val make_cryptographic_safe : unit -> string

  module Table : Map.S with type key = string
  module Set : Set.S with type elt = string
  module Map : Map.S with type key = string

end

module Url : sig

  type t = string
  type uri = string

  val make_absolute_url :
      https:bool -> host:string -> port:int -> uri -> t

  type path = string list

  val remove_dotdot : path -> path
  val remove_end_slash : string -> string
  (* val remove_internal_slash : path -> path *)
  (* val change_empty_list : path -> path *)
  val add_end_slash_if_missing : path -> path
  val remove_slash_at_end : path -> path
  val remove_slash_at_beginning : path -> path
  (* val recursively_remove_slash_at_beginning : path -> path *)
  val is_prefix_skip_end_slash : string list -> string list -> bool

  val string_of_url_path : encode:bool -> path -> uri
  val fixup_url_string : t -> t
  val parse : t ->
    bool option * string option * int option *
      string * string list * string option *
      (string * string) list Lazy.t

  (* val make_encoded_parameters : (string * string) list -> string *)

  val encode : ?plus:bool -> string -> string
  val decode : ?plus:bool -> string -> string

end

module Ip_address : sig

  type t =
    | IPv4 of int32
    | IPv6 of int64 * int64

  (* exception Invalid_ipaddress of string *)

  val parse : string -> t * (t option)
  val match_ip : t * (t option) -> t -> bool
  (* val network_of_ip : ip:t -> mask:t -> t *)

  exception No_such_host

  (* val inet6_addr_loopback : t *)

  val get_inet_addr : string -> Unix.inet_addr Lwt.t

  (* val getnameinfo : Unix.inet_addr -> int -> string Lwt.t *)

end

module Filename : sig

  include module type of Filename

  (* val basename : string -> string *)
  (* val extension : string -> string *)
  val extension_no_directory : string -> string

end

module Printexc : sig

  include module type of Printexc

  val register_exn_printer : ((exn -> string) -> exn -> string) -> unit

end

val debug : string -> unit
