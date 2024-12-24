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
   Cache.
   Association tables (from any kind of database)
   that keep the most recently used values in memory.
   It is also possible to set a maximum lifetime for data in the cache.

   It is based on a structure of doubly linked lists with maximum size,
   that keeps only the mostly recently used values first, if you call the [up]
   function each time you use a value.
   (Insertion, remove and "up" in time 1).
   This structure is exported, so that it can be used in other cases.

   Not (preemptive) thread safe.

   @author Vincent Balat
   @author Raphaël Proust (adding timers)
*)

module Make : functor
    (A : sig
       type key
       type value
     end)
    -> sig
  (** [new cache finder ?timer size] creates a cache object where [finder]
        is the function responsible for retrieving non-cached data, [timer]
        (if any) is the life span of cached values (in seconds) (values in the
        cache are removed after their time is up) and [size] is the upper
        bound to the number of simultaneoulsy cached elements.

        Whenever a value is found (using [find] method), it's lifespan is set
        to [timer] (or not if the cache is not time bounded). If the value was
        already cached, it's lifespan is reset to [timer].

        Using [timer] allow one to create a cache
        bounded both in space and time. It is to be noted that real lifespan
        of values is always slightly greater than [timer]. *)
  class cache : (A.key -> A.value Lwt.t) -> ?timer:float -> int -> object
    method find : A.key -> A.value Lwt.t
    (** Find the cached value associated to the key, or binds this
            value in the cache using the function [finder] passed as argument
            to [create], and returns this value *)

    method find_in_cache : A.key -> A.value
    (** Find the cached value associated to the key. Raises [Not_found]
            if the key is not present in the cache *)

    method remove : A.key -> unit
    method add : A.key -> A.value -> unit
    method clear : unit -> unit
    method size : int
  end
end

val clear_all_caches : unit -> unit
(** Clear the contents of all the existing caches *)

(** Doubly-linked lists with maximum number of entries,
    and (possibly) limited lifespan for entries. *)
module Dlist : sig
  type 'a t
  type 'a node

  val create : ?timer:float -> int -> 'a t
  (** Create a dlist. It takes the maximum length of the list as
      parameter. The optional [?timer] parameter sets a maximum
      lifetime for elements (in seconds). *)

  val add : 'a -> 'a t -> 'a option
  (** Adds an element to the list,
      and possibly returns the element that has been removed if the maximum
      size was exceeded. *)

  val remove : 'a node -> unit
  (** Removes an element from its list.
      If it is not in a list, it does nothing.
      If it is in a list, it calls the finaliser, then removes the element.
      If the finaliser fails with an exception,
      the element is removed and the exception is raised again.
  *)

  val up : 'a node -> unit
  (** Removes the element from its list without finalising,
      then adds it as newest. *)

  val newest : 'a t -> 'a node option
  val oldest : 'a t -> 'a node option
  val size : 'a t -> int
  val maxsize : 'a t -> int
  val value : 'a node -> 'a

  val get_timer : 'a t -> float option
  (** returns the timer of the Dlist *)

  val list_of : 'a node -> 'a t option
  (** The list to which the node belongs *)

  val remove_n_oldest : 'a t -> int -> 'a list
  (** remove the n oldest values (or less if the list is not long enough) ;
      returns the list of removed values *)

  val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** fold over the elements from the cache starting from the newest
      to the oldest *)

  val fold_back : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** fold over the elements from the cache starting from the oldest
      to the newest *)

  val lwt_fold : ('b -> 'a -> 'b Lwt.t) -> 'b -> 'a t -> 'b Lwt.t
  (** lwt version of fold *)

  val lwt_fold_back : ('b -> 'a -> 'b Lwt.t) -> 'b -> 'a t -> 'b Lwt.t
  (** lwt version of fold_back *)

  val move : 'a node -> 'a t -> 'a option
  (** Move a node from one dlist to another one, without finalizing.
      If one value is removed from the destination list (because its
      maximum size is reached), it is returned (after finalisation). *)

  val set_maxsize : 'a t -> int -> 'a list
  (** change the maximum size ;
      returns the list of removed values, if any.
  *)

  val set_finaliser_before : ('a node -> unit) -> 'a t -> unit
  (** set a function to be called automatically on a piece of data
      just before it disappears from the list
      (either by explicit removal or because the maximum size is exceeded) *)

  val get_finaliser_before : 'a t -> 'a node -> unit
  val add_finaliser_before : ('a node -> unit) -> 'a t -> unit

  val set_finaliser_after : ('a node -> unit) -> 'a t -> unit
  (** set a function to be called automatically on a piece of data
      just after it disappears from the list
      (either by explicit removal or because the maximum size is exceeded) *)

  val get_finaliser_after : 'a t -> 'a node -> unit
  val add_finaliser_after : ('a node -> unit) -> 'a t -> unit
end
