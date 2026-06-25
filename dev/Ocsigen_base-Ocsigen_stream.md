
# Module `Ocsigen_base.Ocsigen_stream`

```ocaml
exception Interrupted of exn
```
```ocaml
exception Cancelled
```
```ocaml
exception Already_read
```
```ocaml
exception Finalized
```
Streams are a means to read data block by block

```ocaml
type 'a stream
```
```ocaml
type 'a step = private 
  | Finished of 'a stream option
  | Cont of 'a * 'a stream
```
A stream may be composed by several substreams. Thus a stream is either something that contains the current buffer and a function to retrieve the following data, or a finished stream with possibly another stream following.

```ocaml
type 'a t
```
```ocaml
type outcome = [ 
  | `Success
  | `Failure
 ]
```
```ocaml
val make : ?finalize:(outcome -> unit Lwt.t) -> (unit -> 'a step Lwt.t) -> 'a t
```
creates a new stream

```ocaml
val get : 'a t -> 'a stream
```
call this function if you decide to start reading a stream.

raises [`Already_read`](./#exception-Already_read) if the stream has already been read.
```ocaml
val next : 'a stream -> 'a step Lwt.t
```
get the next step of a stream. Fails with `Interrupted e` if reading the thread failed with exception `e`, and with `Cancelled` if the thread has been cancelled.

```ocaml
val empty : (unit -> 'a step Lwt.t) option -> 'a step Lwt.t
```
creates an empty step. The parameter is the following substream, if any.

```ocaml
val cont : 'a -> (unit -> 'a step Lwt.t) -> 'a step Lwt.t
```
creates a non empty step.

```ocaml
val add_finalizer : 'a t -> (outcome -> unit Lwt.t) -> unit
```
Add a finalizer function. In the current version, finalizers must be called manually.

```ocaml
val finalize : 'a t -> outcome -> unit Lwt.t
```
Finalize the stream. This function must be called explicitly after reading the stream, otherwise finalizers won't be called.

```ocaml
val cancel : 'a t -> unit Lwt.t
```
Cancel the stream, i.e. read the stream until the end, without decoding. Further tries to read on the stream will fail with exception [`Ocsigen_stream.Cancelled`](./#exception-Cancelled)

```ocaml
val consume : 'a t -> unit Lwt.t
```
Consume without cancelling. Read the stream until the end, without decoding.

```ocaml
exception Stream_too_small
```
possibly with the size of the stream

```ocaml
exception Stream_error of string
```
```ocaml
exception String_too_large
```
```ocaml
val string_of_stream : int -> string stream -> string Lwt.t
```
Creates a string from a stream. The first argument is the upper limit of the string length

```ocaml
val enlarge_stream : string step -> string step Lwt.t
```
Read more data in the buffer

```ocaml
val stream_want : string step -> int -> string step Lwt.t
```
`stream_want s len` Returns a stream with at least len bytes in the buffer if possible

```ocaml
val current_buffer : string step -> string
```
Returns the value of the current buffer

```ocaml
val skip : string step -> int64 -> string step Lwt.t
```
Skips data. Raises `Stream_too_small (Some size)` if the stream is too small, where `size` is the size of the stream.

```ocaml
val substream : string -> string step -> string step Lwt.t
```
Cut the stream at the position given by a string delimiter

```ocaml
val of_file : string -> string t
```
returns a stream reading from a file. Do not forget to finalize the stream to close the file.

```ocaml
val of_string : string -> string t
```
returns a stream containing a string.

```ocaml
val of_cohttp_body : Cohttp_lwt.Body.t -> string t
```
Convert a `Lwt_stream.t` to an [`Ocsigen_stream.t`](./#type-t).

```ocaml
module StringStream : sig ... end
```