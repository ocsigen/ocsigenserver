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

(**
   Buffer pools.

   Not (preemptive) thread safe.

   @author Mauricio Fernandez
*)

(** [make_buffer_pool ?min_size ?max_size make capacity]
  * returns two functions [`Round_up f] and [`Exact g] that are invoked as
  * in [f wanted_size] and return a buffer of the wanted size and a function
  * to release the buffer and return it to the pool. These functions do not
  * block and will return a buffer immediately.
  *
  * [f] will round up the size to a power of two. [g] will not, and will thus
  * allocate fresh buffers if invoked with sizes that are not powers of two.
  *
  * [make_buffer_pool] is used as follows:
  *
  * {[
  *   let `Round_up get_buf, `Exact get_buf_exact =
  *     make_buffer_pool ?min_size ?max_size allocate_buffer
  *     (fun n -> how_many_buffers_to_keep_at_most n)
  *
  *   ...
  *   let b, release = get_buf 4096 in
  *   ...
  *
  *   release ()
  * ]}
  *
  * The release function can be invoked any number of times (the second and
  * later calls are NOPs). If not invoked, the buffer will not be returned to
  * the pool, but this is usually harmless (it only means there will be more
  * allocation later in time).
  *
  * The returned buffers must not be used after they have been released, as
  * they might have been reused at that point.
  *
  * @param min_size buffers of size under [min_size] are not pooled (and
  * always freshly allocated with [make size] (default: 64)
  *
  * @param max_size buffers of size over [max_size] are not pooled (and
  * always freshly allocated with [make size] (default: 65536)
  *
  * @param capacity is used to compute how many buffers to retain at most (as a
  * function of the corresponding size). If [capacity n] returns [m], that
  * means that the the pool will hold at most [m] buffers. Note that buffers
  * are allocated lazily (only when the pool is empty), and the actual number
  * of buffers (of a given size) allocated depends on how many are used
  * simultaneously (before being released).
  * *)

val make_buffer_pool :
  ?min_size:int ->
  ?max_size:int ->
  (int -> 'a) ->
  (int -> int) ->
  [`Round_up of int -> 'a * (unit -> unit) ] *
  [`Exact of int -> 'a * (unit -> unit) ]

(** {2 Buffer allocation against internal Ocsigen pools.}
  *
  * Pooling can be disabled altogether (for benchmark or other purposes) by
  * setting the [OCSIGEN_DISABLE_BUFFER_POOL] environment variable.
  * *)
val get_bytes           : int -> Bytes.t * (unit -> unit)
val get_bytes_exact     : int -> Bytes.t * (unit -> unit)
val get_lwt_bytes       : int -> Lwt_bytes.t * (unit -> unit)
val get_lwt_bytes_exact : int -> Lwt_bytes.t * (unit -> unit)
