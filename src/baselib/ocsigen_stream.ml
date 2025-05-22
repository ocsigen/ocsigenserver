open Eio.Std

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

open Ocsigen_lib

exception Interrupted of exn
exception Cancelled
exception Already_read
exception Finalized

type 'a stream = 'a step Lazy.t
(** Forcing a [stream] performs effects. *)

and 'a step =
  | Finished of 'a stream option
  (* If there is another stream following
                                    (useful for substreams) *)
  | Cont of 'a * 'a stream
(* Current buffer, what follows *)

type outcome = [`Success | `Failure]

type 'a t =
  { mutable stream : 'a stream
  ; mutable in_use : bool
  ; mutable finalizer : outcome -> unit }

let net_buffer_size = ref 8192
let set_net_buffer_size i = net_buffer_size := i

let empty follow =
  match follow with
  | None -> Finished None
  | Some st -> Finished (Some (Lazy.from_fun st))

let cont stri f = Cont (stri, Lazy.from_fun f)

let make ?finalize:(g = fun _ -> ()) f =
  {stream = Lazy.from_fun f; in_use = false; finalizer = g}

let next = Lazy.force

let rec get_aux st =
  lazy
    (match Lazy.force st.stream with
    | e -> (
      match e with
      | Cont (s, rem) ->
          st.stream <- rem;
          Cont (s, get_aux st)
      | _ -> e)
    | exception e ->
        st.stream <- lazy (raise e);
        raise (Interrupted e))

let get st =
  if st.in_use then raise Already_read;
  st.in_use <- true;
  get_aux st

(** read the stream until the end, without decoding *)
let rec consume_aux st =
  let e = next st in
  match e with
  | Cont (_, f) -> consume_aux f
  | Finished (Some ss) -> consume_aux ss
  | Finished None -> ()

let cancel st =
  let st' = st.stream in
  st.stream <- lazy (raise Cancelled);
  consume_aux st'

let consume st = consume_aux st.stream

let finalize st status =
  let f = st.finalizer in
  st.finalizer <- (fun _ -> ());
  f status;
  st.stream <- lazy (raise Finalized)

let add_finalizer st g =
  let f = st.finalizer in
  st.finalizer <- (fun status -> f status; g status)

(****)

(** String streams *)

exception Stream_too_small
exception Stream_error of string
exception String_too_large

let string_of_stream m s =
  let buff = Buffer.create (m / 4) in
  let rec aux i s =
    match next s with
    | Finished _ -> buff
    | Cont (s, f) ->
        let i = i + String.length s in
        if i > m
        then raise String_too_large
        else (Buffer.add_string buff s; aux i f)
  in
  Buffer.contents (aux 0 s)

let enlarge_stream = function
  | Finished _a -> raise Stream_too_small
  | Cont (s, f) -> (
      let long = String.length s in
      let max = !net_buffer_size in
      if long >= max
      then raise Input_is_too_large
      else
        let e = next f in
        match e with
        | Finished _ -> raise Stream_too_small
        | Cont (r, ff) ->
            let long2 = String.length r in
            let long3 = long + long2 in
            let new_s = s ^ r in
            if long3 <= max
            then Cont (new_s, ff)
            else
              let long4 = long3 - max in
              cont (String.sub new_s 0 max) (fun () ->
                Cont (String.sub new_s max long4, ff)))

let rec stream_want s len =
  (* returns a stream with at least len bytes read if possible *)
  match s with
  | Finished _ -> s
  | Cont (stri, _f) -> (
      if String.length stri >= len
      then s
      else
        match
          try
            let r = enlarge_stream s in
            `OK r
          with
          | Stream_too_small -> `Too_small
          | e -> raise e
        with
        | `OK r -> stream_want r len
        | `Too_small -> s)

let current_buffer = function
  | Finished _ -> raise Stream_too_small
  | Cont (s, _) -> s

let rec skip s k =
  match s with
  | Finished _ -> raise Stream_too_small
  | Cont (s, f) ->
      let len = String.length s in
      let len64 = Int64.of_int len in
      if Int64.compare k len64 <= 0
      then
        let k = Int64.to_int k in
        Cont (String.sub s k (len - k), f)
      else
        let s = enlarge_stream (Cont ("", f)) in
        skip s (Int64.sub k len64)

let substream delim s =
  let ldelim = String.length delim in
  if ldelim = 0
  then raise (Stream_error "Empty delimiter")
  else
    let rdelim = Re.Pcre.(regexp (quote delim)) in
    let rec aux = function
      | Finished _ -> raise Stream_too_small
      | Cont (s, f) as stre -> (
          let len = String.length s in
          if len < ldelim
          then aux (enlarge_stream stre)
          else
            try
              let p, (_ : 'groups) =
                Ocsigen_lib.Netstring_pcre.search_forward rdelim s 0
              in
              cont (String.sub s 0 p) (fun () ->
                empty (Some (fun () -> Cont (String.sub s p (len - p), f))))
            with Not_found ->
              let pos = len + 1 - ldelim in
              cont (String.sub s 0 pos) (fun () ->
                match next f with
                | Finished _ -> raise Stream_too_small
                | Cont (s', f') ->
                    aux (Cont (String.sub s pos (len - pos) ^ s', f'))))
    in
    aux s

(*****************************************************************************)

(*VVV Is it the good place for this? *)

let of_file filename =
  let fd = Unix.openfile filename [Unix.O_RDONLY; Unix.O_NONBLOCK] 0o666 in
  let ch =
    (Eio_unix.Net.import_socket_stream
       ~sw:(Stdlib.Option.get (Fiber.get Ocsigen_lib.current_switch))
       ~close_unix:true fd
     : [`R | `Flow | `Close] r)
  in
  let buf = Cstruct.create 1024 in
  let rec aux () =
    let n = Eio.Flow.single_read ch buf in
    if n = 0
    then empty None
    else
      (* Streams should be immutable, thus we always make a copy
         of the buffer *)
      cont (Cstruct.to_string ~len:n buf) aux
  in
  make ~finalize:(fun _ -> Unix.close fd) aux

let of_string s = make (fun () -> cont s (fun () -> empty None))

let of_eio_flow body =
  let buf = Cstruct.create !net_buffer_size in
  let rec aux () =
    match Eio.Flow.single_read body buf with
    | exception End_of_file -> empty None
    | len -> cont (Cstruct.to_string ~len buf) aux
  in
  make aux

let of_cohttp_body = of_eio_flow

module StringStream = struct
  type out = string t
  type m = (string stream -> string step) Lazy.t

  let empty : m = lazy (fun c -> Lazy.force c)

  let concat (m : m) (f : m) : m =
    lazy (fun c -> Lazy.force m (lazy (Lazy.force f c)))

  let put (s : string) : m = lazy (fun c -> Cont (s, c))

  let make_stream (m : m) : string stream =
    lazy (Lazy.force m (lazy (Finished None)))

  let make (m : m) : out = make (fun () -> Lazy.force (make_stream m))
end
