(* Copyright (C) 2015 Mauricio Fernandez
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

module POOL :
sig
  type 'a t

  val make : (unit -> 'a) -> int -> 'a t
  val take : 'a t -> 'a
  val give_back : 'a t -> 'a -> unit
end =
struct
  type 'a t =
      {
        create   : unit -> 'a;
        q        : 'a Stack.t;
        capacity : int;
        mutable q_size : int;
      }

  let make create capacity =
    { create; capacity;
      q_size = 0;
      q = Stack.create ();
    }

  let take t =
    if Stack.is_empty t.q then
       t.create ()
    else
      let x = Stack.pop t.q in
        t.q_size <- t.q_size - 1;
        x

  let give_back t x =
    if t.q_size < t.capacity then begin
      Stack.push x t.q;
      t.q_size <- t.q_size + 1;
    end
end

let round_to_pow2 n =
  let m = ref 1 in
    while !m < n do
      m := !m * 2;
    done;
    !m

let is_pow2 n =
  let m = n lor (n lsr 1) in
  let m = m lor (m lsr 2) in
  let m = m lor (m lsr 4) in
  let m = m lor (m lsr 8) in
  let m = m lor (m lsr 16) in
    n land (m lsr 1) = 0

let make_buffer_pool ?(min_size = 64) ?(max_size = 65536) create capacity =
  let h = Hashtbl.create 13 in

  let get_pool size =
    try
      Hashtbl.find h size
    with Not_found ->
      let p = POOL.make (fun () -> create size) (capacity size) in
        Hashtbl.add h size p;
        p in

  let get ~exact size =
    if (exact && not (is_pow2 size)) || size < min_size || size > max_size then
      (create size, (fun () -> ()))
    else
      let p = get_pool (round_to_pow2 size) in
      let x = POOL.take p in

      let released = ref false in

      let release () =
        if not !released then begin
          released := true;
          POOL.give_back p x
        end
      in
        (x, release)
  in
    (`Round_up (get ~exact:false), `Exact (get ~exact:true))

let `Round_up get_bytes, `Exact get_bytes_exact =
  make_buffer_pool Bytes.create (fun _ -> 256)

let `Round_up get_lwt_bytes, `Exact get_lwt_bytes_exact =
  make_buffer_pool Lwt_bytes.create (fun _ -> 256)

let dummy_get_bytes n       = (Bytes.create n, (fun () -> ()))
let dummy_get_lwt_bytes   n = (Lwt_bytes.create n, (fun () -> ()))

let get_bytes, get_bytes_exact =
  try
    ignore (Unix.getenv "OCSIGEN_DISABLE_BUFFER_POOL");
    (dummy_get_bytes, dummy_get_bytes)
  with Not_found ->
    (get_bytes, get_bytes_exact)

let get_lwt_bytes, get_lwt_bytes_exact =
  try
    ignore (Unix.getenv "OCSIGEN_DISABLE_BUFFER_POOL");
    (dummy_get_lwt_bytes, dummy_get_lwt_bytes)
  with Not_found ->
    (get_lwt_bytes, get_lwt_bytes_exact)
