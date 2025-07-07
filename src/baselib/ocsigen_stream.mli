(* Ocsigen
 * ocsigen_stream.ml Copyright (C) 2005 Vincent Balat
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

exception Interrupted of exn
exception Cancelled
exception Already_read
exception Finalized

(** Streams are a means to read data block by block *)

type 'a stream

(** A stream may be composed by several substreams.
    Thus a stream is either something that contains the current buffer and
    a function to retrieve the following data,
    or a finished stream with possibly another stream following.
*)
type 'a step = private Finished of 'a stream option | Cont of 'a * 'a stream

type 'a t
type outcome = [`Success | `Failure]

val make : ?finalize:(outcome -> unit) -> (unit -> 'a step) -> 'a t
(** creates a new stream *)

val get : 'a t -> 'a stream
(** call this function if you decide to start reading a stream.
    @raise Already_read if the stream has already been read. *)

val next : 'a stream -> 'a step
(** get the next step of a stream.
    Fails with [Interrupted e] if reading the thread failed with exception [e],
    and with [Cancelled] if the thread has been cancelled. *)

val empty : (unit -> 'a step) option -> 'a step
(** creates an empty step. The parameter is the following substream, if any. *)

val cont : 'a -> (unit -> 'a step) -> 'a step
(** creates a non empty step. *)

val add_finalizer : 'a t -> (outcome -> unit) -> unit
(** Add a finalizer function. In the current version,
    finalizers must be called manually. *)

val finalize : 'a t -> outcome -> unit
(** Finalize the stream. This function must be called explicitly after reading
    the stream, otherwise finalizers won't be called. *)

val cancel : 'a t -> unit
(** Cancel the stream, i.e. read the stream until the end, without decoding.
    Further tries to read on the stream will fail with exception
    {!Ocsigen_stream.Cancelled}
*)

val consume : 'a t -> unit
(** Consume without cancelling.
    Read the stream until the end, without decoding. *)

exception Stream_too_small
(** possibly with the size of the stream *)

exception Stream_error of string
exception String_too_large

val string_of_stream : int -> string stream -> string
(** Creates a string from a stream. The first argument is the upper limit of the
    string length *)

val enlarge_stream : string step -> string step
(** Read more data in the buffer *)

val stream_want : string step -> int -> string step
(** [stream_want s len] Returns a stream with at least len
    bytes in the buffer if possible *)

val current_buffer : string step -> string
(** Returns the value of the current buffer *)

val skip : string step -> int64 -> string step
(** Skips data. Raises [Stream_too_small (Some size)]
    if the stream is too small, where [size] is the size of the stream. *)

val substream : string -> string step -> string step
(** Cut the stream at the position given by a string delimiter *)

val of_file : string -> string t
(** returns a stream reading from a file.
    Do not forget to finalize the stream to close the file.
*)

val of_string : string -> string t
(** returns a stream containing a string. *)

val of_cohttp_body : Cohttp_lwt.Body.t -> string t
(** Convert a {!Lwt_stream.t} to an {!Ocsigen_stream.t}. *)

module StringStream : sig
  type out = string t
  (** Interface for stream creation (for tyxml) *)

  type m

  val make : m -> out

  val empty : m
  (** Create an empty stream *)

  val put : string -> m
  (** Create a stream with one element *)

  val concat : m -> m -> m
  (** Concatenate two stream *)
end

(**/**)

(* Small hack that will allow us to move [Ocsigen_config] out of
   baselib. Not super-pretty. *)
val set_net_buffer_size : int -> unit
