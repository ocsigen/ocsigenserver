(* Ocsigen
 * http://www.ocsigen.org
 * Module ocsipersist.mli
 * Copyright (C) 2007 Vincent Balat - Gabriel Kerneis 
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


(** Persistent data on hard disk. *)

(**
   There are currently three implementations of this module,
   one using a DBM database, on the PostgreSQL database,
   and one using SQLITE. Link the one your want with your program.
*)


(*****************************************************************************)
(** {2 Persistent references} *)
(** When launching the program, if the value exists on hard disk,
    it is loaded, otherwise it is initialised to the default value *)

(** Type of persistent data *)
type 'a t

(** Data are divided into stores.
    Create one store for your project, where you will save all your data. *)
type store

(** Open a store (and create it if it does not exist)  *)
val open_store : string -> store Lwt.t

val make_persistent :
  store:store -> name:string -> default:'a -> 'a t Lwt.t
(** [make_persistent store name default] find a persistent value
    named [name] in store [store]
    from database, or create it with the default value [default] if it
    does not exist. *)

val make_persistent_lazy :
  store:store -> name:string -> default:(unit -> 'a) -> 'a t Lwt.t
(** Same as make_persistent but the default value is evaluated only
    if needed
*)

val make_persistent_lazy_lwt :
  store:store -> name:string -> default:(unit -> 'a Lwt.t) -> 'a t Lwt.t
(** Lwt version of make_persistent_lazy.
*)

val get : 'a t -> 'a Lwt.t
(** [get pv] gives the value of [pv] *)

val set : 'a t -> 'a -> unit Lwt.t
(** [set pv value] sets a persistent value [pv] to [value] *)


(** {2 Persistent tables} *)

(* FUNCTORIAL INTERFACE ******************************************************)

type internal

module type TABLE_CONF = sig
  val name : string
end

module type COLUMN = sig
  type t
  val column_type : string
  val encode : t -> internal
  val decode : internal -> t
end

module type TABLE = sig
  type key
  type value

  val name : string
  val find : key -> value Lwt.t
  val add : key -> value -> unit Lwt.t
  val replace_if_exists : key -> value -> unit Lwt.t
  val remove : key -> unit Lwt.t
  val modify_opt : key -> (value option -> value option) -> unit Lwt.t
  val length : unit -> int Lwt.t
  val iter :
    ?count:int64 ->
    ?gt:key -> ?geq:key -> ?lt:key -> ?leq:key ->
    (key -> value -> unit Lwt.t) -> unit Lwt.t
  val fold :
    ?count:int64 ->
    ?gt:key -> ?geq:key -> ?lt:key -> ?leq:key ->
    (key -> value -> 'a -> 'a Lwt.t) -> 'a -> 'a Lwt.t
  val iter_block :
    ?count:int64 ->
    ?gt:key -> ?geq:key -> ?lt:key -> ?leq:key ->
    (key -> value -> unit) -> unit Lwt.t
end

module Table (T : TABLE_CONF) (Key : COLUMN) (Value : COLUMN)
       : TABLE with type key = Key.t and type value = Value.t

module Column : sig
  module String : COLUMN with type t = string
  module Float : COLUMN with type t = float
  module Marshal (C : sig type t end) : COLUMN with type t = C.t
end

(*****************************************************************************)

(** Type of persistent table *)
type 'value table

(** returns the name of the table  *)
val table_name : 'value table -> string Lwt.t

(** Open a table (and create it if it does not exist)  *)
val open_table : string -> 'value table Lwt.t

val find : 'value table -> string -> 'value Lwt.t
(** [find table key] gives the value associated to [key].
    Fails with [Not_found] if not found. *)

val add : 'value table -> string -> 'value -> unit Lwt.t
(** [add table key value] associates [value] to [key].
    If the database already contains data associated with [key],
    that data is discarded and silently replaced by the new data.
*)

val replace_if_exists : 'value table -> string -> 'value -> unit Lwt.t
(** [replace_if_exists table key value]
    associates [value] to [key] only if [key] is already bound.
    If the database does not contain any data associated with [key],
    fails with [Not_found].
*)

val remove : 'value table -> string -> unit Lwt.t
(** [remove table key] removes the entry in the table if it exists *)

val length : 'value table -> int Lwt.t
(** Size of a table. *)

val iter_step : (string -> 'a -> unit Lwt.t) -> 'a table -> unit Lwt.t
(** Important warning: this iterator may not iter on all data of the table
    if another thread is modifying it in the same time. Nonetheless, it should
    not miss more than a very few data from time to time, except if the table
    is very old (at least 9 223 372 036 854 775 807 insertions).
*)

val iter_table : (string -> 'a -> unit Lwt.t) -> 'a table -> unit Lwt.t
(** Legacy interface for iter_step *)

val fold_step : (string -> 'a -> 'b -> 'b Lwt.t) ->
  'a table -> 'b -> 'b Lwt.t
(** Important warning: this iterator may not iter on all data of the table
    if another thread is modifying it in the same time. Nonetheless, it should
    not miss more than a very few data from time to time, except if the table
    is very old (at least 9 223 372 036 854 775 807 insertions).
*)

val fold_table : (string -> 'a -> 'b -> 'b Lwt.t) ->
  'a table -> 'b -> 'b Lwt.t
(** Legacy interface for fold_step *)

(**/**)
val iter_block : (string -> 'a -> unit) -> 'a table -> unit Lwt.t
(** MAJOR WARNING: Unlike iter_step, this iterator won't miss any
    entry and will run in one shot. It is therefore more efficient, BUT:
    it will lock the WHOLE database during its execution,
    thus preventing ANYBODY from accessing it (including the function f
    which is iterated).
    As a consequence : you MUST NOT use any function from ocsipersist in f,
    otherwise you would lock yourself and everybody else ! Be VERY cautious.
*)
