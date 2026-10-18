# Module `Ocsigen.Connection_guard`

Bounds on how long a connection may wait for its client.

Each connection is watched by a guard, which knows what the connection is waiting for (its [`phase`](./#val-phase)) and how long it has been waiting for the client. A single ticker, run once a second for all the connections, expires those that exceed their [`limits`](./#type-limits). Reads and writes cost no timer: they only update the counters of their guard.

Only the time spent waiting for the client counts: the time a read or a write is pending. A request handler that takes long to produce its response, or to read the body of its request, is never cut, and neither is an Eliom comet request waiting for an event.

```ocaml
exception Timed_out
```
The failure of a read, a write or an operation on an expired connection.

```ocaml
type limits = {
  timeout : int; (* In seconds. It bounds the TLS handshake, the reception of the headers of a request (in total, from the end of the handshake, or from the first byte after an idle period), each read of the body of a request and each write of a response (inactivity), and the end of a connection. 0 disables these bounds, and the minimum rate. *)
  keepalive_timeout : int; (* In seconds, how long a connection may stay idle between two requests. 0 disables this bound. *)
  min_rate : int; (* In bytes per second, the minimum rate of the body of a request, and of a response the client reads, once the client has had timeout seconds of grace. It keeps a client from holding a connection by sending its body, or reading the response, a few bytes at a time. 0 disables this bound, and so does a timeout of 0. *)
}
```
```ocaml
type phase = 
  | Handshake (* The TLS handshake. *)
  | Headers (* Receiving the headers of a request. *)
  | Idle (* Waiting for the next request. *)
  | Request (* Handling a request: reading its body, producing and sending the response. *)
  | Closing (* Ending the connection. *)
```
```ocaml
type t
```
The guard of a connection.

```ocaml
val create : limits -> phase -> on_expiry:(phase -> unit) -> t
```
`create limits phase ~on_expiry` is a guard, watched by the ticker from now on, for a connection in `phase`. When the connection exceeds `limits`, `on_expiry` is called with the phase it was in, then the pending operations of the guard fail with [`Timed_out`](./#exception-Timed_out), and so do all those that follow.

A connection that stays [`Idle`](./#type-phase.Idle) longer than the keep-alive timeout is not expired but closed, its client having done nothing wrong: its reads fail with [`Timed_out`](./#exception-Timed_out), the pending one and the later ones, and it enters [`Closing`](./#type-phase.Closing), where the server can still end it properly (with a TLS close\_notify, for instance) within the timeout. `on_expiry` is not called then.

A connection is also closed that way, the oldest first, when the server is full (see [`set_capacity`](./#val-set_capacity)) and its client is not using it: it is [`Idle`](./#type-phase.Idle), in the TLS [`Handshake`](./#type-phase.Handshake), or in [`Headers`](./#type-phase.Headers) without having started a request. A closed handshake fails with [`Timed_out`](./#exception-Timed_out). This happens when a new connection fills the server, so that the next client can connect, and at a tick, for connections that became unused once the server was full.

```ocaml
val set_capacity : int -> unit
```
`set_capacity n` records that the server stops accepting connections once `n` are open. There is no capacity by default.

```ocaml
val release : t -> unit
```
`release t` stops watching `t`, once its connection has ended.

```ocaml
val set_phase : t -> phase -> unit
```
`set_phase t phase` records that the connection of `t` is now in `phase`. Entering [`Request`](./#type-phase.Request) resets the counters of the body and of the response. A read of at least one byte while [`Idle`](./#type-phase.Idle) enters [`Headers`](./#type-phase.Headers) by itself. Once the connection has expired, or has been closed, its phase no longer changes.

```ocaml
val phase : t -> phase
```
```ocaml
val expired : t -> bool
```
```ocaml
val read : 
  t ->
  (Lwt_bytes.t -> int -> int -> int Lwt.t) ->
  Lwt_bytes.t ->
  int ->
  int ->
  int Lwt.t
```
`read t f` is the read function `f`, watched by `t`, in the form expected by `Lwt_io.make`.

```ocaml
val write : 
  t ->
  (Lwt_bytes.t -> int -> int -> int Lwt.t) ->
  Lwt_bytes.t ->
  int ->
  int ->
  int Lwt.t
```
`write t f` is the write function `f`, watched by `t`.

```ocaml
val run : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
```
`run t f` is `f ()`, failing with [`Timed_out`](./#exception-Timed_out) if the connection expires meanwhile. It is meant for the operations bounded by their phase as a whole, such as the TLS handshake.

```ocaml
val start : unit -> unit
```
`start ()` starts the ticker, if it is not running yet.

```ocaml
val tick : unit -> unit
```
`tick ()` advances the clock of the guards by one second and expires the connections that exceeded their limits. The ticker calls it every second; it is exposed for tests, which drive the clock themselves instead of calling [`start`](./#val-start).

```ocaml
module Per_address : sig ... end
```
Number of simultaneous connections from a single client.
