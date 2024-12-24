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

   @author Vincent Balat
   @author Raphaël Proust (adding timers)
*)

let ( >>= ) = Lwt.bind

module Dlist : sig
  type 'a t
  type 'a node

  val create : ?timer:float -> int -> 'a t
  val add : 'a -> 'a t -> 'a option
  val newest : 'a t -> 'a node option
  val oldest : 'a t -> 'a node option

  val remove : 'a node -> unit
  (** Removes an element from its list.
      If the element is not in a list, it does nothing.
      If it is in a list, it calls the finaliser, then removes the element.
      If the finaliser fails with an exception,
      the element is removed and the exception is raised again.
  *)

  val up : 'a node -> unit
  (** Removes the element from its list without finalising,
      then adds it as newest. *)

  val size : 'a t -> int
  val maxsize : 'a t -> int

  val get_timer : 'a t -> float option
  (** returns the timer of the Dlist *)

  val value : 'a node -> 'a
  val list_of : 'a node -> 'a t option

  val remove_n_oldest : 'a t -> int -> 'a list
  (** remove the n oldest values ;
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
      returns the list of removed values, if any. *)

  val add_finaliser_before : ('a node -> unit) -> 'a t -> unit
  (** record a function to be called automatically on a piece of data
      just before it disappears from the list
      (either by explicit removal or because the maximum size is exceeded) *)

  val set_finaliser_before : ('a node -> unit) -> 'a t -> unit
  (** replace all finalizers by a new one. Be very careful while using this. *)

  val get_finaliser_before : 'a t -> 'a node -> unit
  (** returns the finalizers. *)

  val add_finaliser_after : ('a node -> unit) -> 'a t -> unit
  (** record a function to be called automatically on a piece of data
      just after it disappears from the list
      (either by explicit removal or because the maximum size is exceeded) *)

  val set_finaliser_after : ('a node -> unit) -> 'a t -> unit
  (** replace all finalizers by a new one. Be very careful while using this. *)

  val get_finaliser_after : 'a t -> 'a node -> unit
  (** returns the finalizers. *)
end = struct
  type 'a node =
    { mutable value : 'a
    ; mutable succ : 'a node option
    ; (* the node added just after *)
      mutable prev : 'a node option
    ; (* the node added just before *)
      mutable mylist : 'a t option
    ; (* the list to which it belongs *)
      mutable collection : float option (* the timestamp for removal *) }

  (* Doubly-linked list with maximum size.
     The field [oldest] is the first
     element that must be removed if the list becomes too long.
  *)
  and 'a t =
    { mutable newest : 'a node option (* None = empty *)
    ; mutable oldest : 'a node option
    ; mutable size : int
    ; mutable maxsize : int
    ; mutable finaliser_before : 'a node -> unit
    ; mutable finaliser_after : 'a node -> unit
    ; time_bound : time_bound option }

  and time_bound = {timer : float; mutable collector : unit Lwt.t option}

  (* Checks (by BY):

     let compute_length c =
      let rec aux i = function
        | Some {prev=p} -> aux (i + 1) p
        | None -> i
      in aux 0 c.newest

     let correct_node n =
      (match n.succ with
         | None -> true
         | Some n' -> n'.prev == Some n) &&
       (match n.prev with
          | None -> true
          | Some n' -> n'.succ == Some n)

     (* Check that a list is correct. To be completed
       1. by adding a check on nodes,
       2. by verifying that newest can be reached from oldest and respectively *)
     let correct_list l =
      (l.size <= l.maxsize) &&
      (compute_length l = l.size) &&
      (match l.oldest with
         | None -> true
         | Some n -> n.prev = None) &&
      (match l.newest with
         | None -> true
         | Some n -> n.succ = None)
  *)

  let create ?timer size =
    { newest = None
    ; oldest = None
    ; size = 0
    ; maxsize = size
    ; finaliser_before = (fun _ -> ())
    ; finaliser_after = (fun _ -> ())
    ; time_bound =
        (match timer with
        | None -> None
        | Some t -> Some {timer = t; collector = None}) }

  (* Remove an element from its list - don't finalise *)
  let remove' node l =
    (* assertion (node.mylist = Some l' with l' == l); *)
    let oldest =
      match l.oldest with Some n when node == n -> node.succ | _ -> l.oldest
    in
    let newest =
      match l.newest with Some n when node == n -> node.prev | _ -> l.newest
    in
    (match node.succ with None -> () | Some s -> s.prev <- node.prev);
    (match node.prev with None -> () | Some s -> s.succ <- node.succ);
    l.oldest <- oldest;
    l.newest <- newest;
    node.mylist <- None;
    l.size <- l.size - 1

  (* Remove an element from its list - and finalise *)
  let remove node =
    match node.mylist with
    | None -> ()
    | Some l as a ->
        (try
           l.finaliser_before node;
           assert (node.mylist == a);
           remove' node l
         with e -> remove' node l; raise e);
        l.finaliser_after node

  (* These next functions are for the collecting thread *)

  (* computing the timestamp for a node *)
  let collect_timer = function
    | {time_bound = Some {timer = t; _}; _} -> Some (t +. Unix.gettimeofday ())
    | {time_bound = None; _} -> None

  (* do collect. We first check if the node is still in the list and then if
   * its collection hasn't been rescheduled ! *)
  let collect dl n =
    match n.mylist with
    | Some l when l == dl -> (
      match n.collection with
      | None -> assert false
      | Some c -> if c < Unix.gettimeofday () then remove n else ())
    | None | Some _ -> ()

  let sleep_until = function
    (*/!\ COOPERATES*)
    | None -> assert false (* collection is set to None and collector to Some *)
    | Some t ->
        let duration = t -. Unix.gettimeofday () in
        if duration <= 0. then Lwt.return () else Lwt_unix.sleep duration

  (* a function to set the collector. *)
  let rec update_collector r =
    match r.time_bound with
    | None (* Not time bounded dlist *) | Some {collector = Some _; _} ->
        () (* Already collecting *)
    | Some ({collector = None; _} as t) -> (
      match r.oldest with
      | None -> () (* Empty dlist *)
      | Some n ->
          t.collector <-
            Some
              ( sleep_until n.collection >>= fun () ->
                collect r n;
                t.collector <- None;
                update_collector r;
                Lwt.return () ))

  (* Add a node that do not belong to any list to a list.
     The fields [succ] and [prev] are overridden.
     If the list is too long, the function returns the oldest value.
     The node added becomes the element [list] of the list *)
  (* do not finalise *)
  (* not exported *)
  let add_node node r =
    assert (node.mylist = None);
    node.mylist <- Some r;
    let res =
      match r.newest with
      | None ->
          node.succ <- None;
          node.prev <- None;
          r.newest <- Some node;
          r.oldest <- r.newest;
          r.size <- 1;
          None
      | Some rl ->
          node.succ <- None;
          node.prev <- r.newest;
          rl.succ <- Some node;
          r.newest <- Some node;
          r.size <- r.size + 1;
          if r.size > r.maxsize then r.oldest else None
    in
    node.collection <- collect_timer r;
    update_collector r;
    res

  let add x l =
    let create_one a =
      {value = a; succ = None; prev = None; mylist = None; collection = None}
    in
    (* create_one not exported *)
    match add_node (create_one x) l with
    | None -> None
    | Some v -> remove v; Some v.value

  let newest a = a.newest
  let oldest a = a.oldest
  let size c = c.size
  let maxsize c = c.maxsize

  let get_timer c =
    match c.time_bound with None -> None | Some tb -> Some tb.timer

  let value n = n.value
  let list_of n = n.mylist

  let up node =
    match node.mylist with
    | None -> ()
    | Some l -> (
      match l.newest with
      | Some n when node == n -> ()
      | _ ->
          remove' node l;
          ignore (add_node node l))

  (* assertion: = None *)
  (* we must not change the physical address => use add_node *)

  let rec remove_n_oldest l n =
    (* remove the n oldest values
                                   (or less if the list is not long enough) ;
                                   returns the list of removed values *)
    if n <= 0
    then []
    else
      match l.oldest with
      | None -> []
      | Some node ->
          let v = node.value in
          remove node;
          (* and finalise! *)
          v :: remove_n_oldest l (n - 1)

  (* Move a node from one dlist to another one, without finalizing *)
  let move node l =
    (match node.mylist with None -> () | Some l -> remove' node l);
    match add_node node l with None -> None | Some v -> remove v; Some v.value

  (* fold over the elements from the newest to the oldest *)
  let lwt_fold f accu {newest; _} =
    match newest with
    | None -> Lwt.return accu
    | Some newest ->
        let rec fold accu node =
          f accu node.value >>= fun accu ->
          match node.prev with
          | None -> Lwt.return accu
          | Some new_node when new_node == newest -> Lwt.return accu
          | Some new_node -> fold accu new_node
        in
        fold accu newest

  (* fold over the elements from the oldest to the newest *)
  let lwt_fold_back f accu {oldest; _} =
    match oldest with
    | None -> Lwt.return accu
    | Some oldest ->
        let rec fold accu node =
          f accu node.value >>= fun accu ->
          match node.succ with
          | None -> Lwt.return accu
          | Some new_node when new_node == oldest -> Lwt.return accu
          | Some new_node -> fold accu new_node
        in
        fold accu oldest

  (* fold over the elements from the newest to the oldest *)
  let fold f accu {newest; _} =
    match newest with
    | None -> accu
    | Some newest ->
        let rec fold accu node =
          let accu = f accu node.value in
          match node.prev with
          | None -> accu
          | Some new_node when new_node == newest -> accu
          | Some new_node -> fold accu new_node
        in
        fold accu newest

  (* fold over the elements from the oldest to the newest *)
  let fold_back f accu {oldest; _} =
    match oldest with
    | None -> accu
    | Some oldest ->
        let rec fold accu node =
          let accu = f accu node.value in
          match node.succ with
          | None -> accu
          | Some new_node when new_node == oldest -> accu
          | Some new_node -> fold accu new_node
        in
        fold accu oldest

  let set_maxsize l m =
    let size = l.size in
    if m >= size
    then (
      l.maxsize <- m;
      [])
    else if m <= 0
    then failwith "Dlist.set_maxsize"
    else
      let ll = remove_n_oldest l (size - m) in
      l.maxsize <- m;
      ll

  let set_finaliser_before f l = l.finaliser_before <- f
  let get_finaliser_before l = l.finaliser_before

  let add_finaliser_before f l =
    let oldf = l.finaliser_before in
    l.finaliser_before <- (fun n -> oldf n; f n)

  let set_finaliser_after f l = l.finaliser_after <- f
  let get_finaliser_after l = l.finaliser_after

  let add_finaliser_after f l =
    let oldf = l.finaliser_after in
    l.finaliser_after <- (fun n -> oldf n; f n)
end

module Weak = Weak.Make (struct
    type t = unit -> unit

    let hash = Hashtbl.hash
    let equal = ( == )
  end)

let clear_all = Weak.create 17

module Make =
functor
  (A : sig
     type key
     type value
   end)
  ->
  struct
    module H = Hashtbl.Make (struct
        type t = A.key

        let equal a a' = a = a'
        let hash = Hashtbl.hash
      end)

    type t =
      { mutable pointers : A.key Dlist.t
      ; mutable table : (A.value * A.key Dlist.node) H.t
      ; finder : A.key -> A.value Lwt.t
      ; clear : unit -> unit
        (* This function clears the cache. It is put inside the
                               cache structure so that it is garbage-collected only when the cache
                               is no longer referenced, as the functions themselves are put inside
                               a weak hash table *)
      }

    let mk ?timer size =
      let ((l, t) as a) = Dlist.create ?timer size, H.create size in
      Dlist.set_finaliser_after (fun n -> H.remove t (Dlist.value n)) l;
      a

    let rec create f ?timer size =
      let rec cache =
        let l, t = mk ?timer size in
        {pointers = l; table = t; finder = f; clear = f_clear}
      and f_clear () = clear cache in
      Weak.add clear_all f_clear; cache

    and clear cache =
      let size = Dlist.maxsize cache.pointers in
      let timer = Dlist.get_timer cache.pointers in
      let l, t = mk ?timer size in
      cache.pointers <- l;
      cache.table <- t

    (* not exported *)
    let poke cache node =
      assert (
        match Dlist.list_of node with
        | None -> false
        | Some l -> cache.pointers == l);
      Dlist.up node

    let find_in_cache cache k =
      let v, node = H.find cache.table k in
      poke cache node; v

    let remove cache k =
      try
        let _v, node = H.find cache.table k in
        assert (
          match Dlist.list_of node with
          | None -> false
          | Some l -> cache.pointers == l);
        Dlist.remove node
      with Not_found -> ()

    (* Add in a cache, under the hypothesis that the value is
       not already in the cache *)
    let add_no_remove cache k v =
      ignore (Dlist.add k cache.pointers);
      match Dlist.newest cache.pointers with
      | None -> assert false
      | Some n -> H.add cache.table k (v, n)

    let add cache k v = remove cache k; add_no_remove cache k v
    let size c = Dlist.size c.pointers

    let find cache k =
      try Lwt.return (find_in_cache cache k)
      with Not_found ->
        cache.finder k >>= fun r ->
        (try
           (* it may have been added during cache.finder *)
           ignore (find_in_cache cache k)
         with Not_found -> add_no_remove cache k r);
        Lwt.return r

    class cache f ?timer size_c =
      let c = create f ?timer size_c in
      object
        method clear () = clear c
        method find = find c
        method add = add c
        method size = size c
        method find_in_cache = find_in_cache c
        method remove = remove c
      end
  end

let clear_all_caches () = Weak.iter (fun f -> f ()) clear_all
