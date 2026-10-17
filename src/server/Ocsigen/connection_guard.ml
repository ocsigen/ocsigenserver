open Lwt.Infix

let section = Logs.Src.create "ocsigen:connection-guard"

exception Timed_out

type limits = {timeout : int; keepalive_timeout : int; min_rate : int}
type phase = Handshake | Headers | Idle | Request | Closing

(* The clock of the guards, in seconds, advanced by [tick]. It is coarse, but
   monotonic and free to read, and it lags behind when the server is overloaded,
   which gives clients more time rather than less. *)
let now = ref 0

(* The reads, or the writes, of a connection. *)
type side =
  { mutable pending : (unit -> unit) option
    (* How to cancel the pending operation, if there is one. *)
  ; mutable since : int (* When the pending operation started. *)
  ; mutable waited : int
    (* Seconds spent on the completed operations since the last reset. *)
  ; mutable bytes : int (* Bytes transferred since the last reset. *) }

type state =
  | Open
  | Closed of side
  (* The server closed this side of the connection: see [close_idle]. *)
  | Expired

type t =
  { id : int
  ; limits : limits
  ; on_expiry : phase -> unit
  ; mutable phase : phase
  ; mutable phase_since : int
  ; mutable state : state
  ; reads : side
  ; writes : side
  ; other : side (* The operations of [run]. *) }

let guards : (int, t) Hashtbl.t = Hashtbl.create 256
let last_id = ref 0
let new_side () = {pending = None; since = 0; waited = 0; bytes = 0}

let create limits phase ~on_expiry =
  incr last_id;
  let t =
    { id = !last_id
    ; limits
    ; on_expiry
    ; phase
    ; phase_since = !now
    ; state = Open
    ; reads = new_side ()
    ; writes = new_side ()
    ; other = new_side () }
  in
  Hashtbl.replace guards t.id t;
  t

let release t = Hashtbl.remove guards t.id
let phase t = t.phase
let expired t = match t.state with Expired -> true | Open | Closed _ -> false

let reset side =
  side.since <- !now;
  side.waited <- 0;
  side.bytes <- 0

let set_phase t phase =
  match t.state with
  | Open -> (
      t.phase <- phase;
      t.phase_since <- !now;
      match phase with
      | Request -> reset t.reads; reset t.writes
      | Handshake | Headers | Idle | Closing -> ())
  | Closed _ | Expired -> ()

(* Whether a client that has transferred [side.bytes] in [waited] seconds of
   waiting is below the minimum rate, past its grace period. A timeout of [0]
   disables the minimum rate too: without a grace period, a client would have to
   send or read something every second. *)
let too_slow limits side waited =
  limits.timeout > 0 && limits.min_rate > 0
  && waited > limits.timeout + (side.bytes / limits.min_rate)

(* Whether the pending operation of [side], if there is one, has been stalled
   too long, or the client is too slow overall. *)
let lagging limits side =
  match side.pending with
  | None -> false
  | Some _ ->
      let stalled = !now - side.since in
      (limits.timeout > 0 && stalled > limits.timeout)
      || too_slow limits side (side.waited + stalled)

(* What becomes of a connection at a tick. The headers of a request, the TLS
   handshake and the end of a connection are bounded in total, and the body of
   a request and each response by their progress: past these bounds, the
   connection expires. An idle connection past the keep-alive timeout is only
   closed, since its client did nothing wrong. Since the clock ticks once a
   second, a bound of [n] seconds is reached between [n] and [n + 2] seconds
   after the connection started to wait: never early. *)
type action = Close_idle | Expire

let action t =
  let limits = t.limits in
  let elapsed = !now - t.phase_since in
  let overdue =
    (match t.phase with
      | Handshake | Headers | Closing ->
          limits.timeout > 0 && elapsed > limits.timeout
      | Idle -> false
      | Request -> lagging limits t.reads)
    || lagging limits t.writes
  in
  match t.phase with
  | _ when overdue -> Some Expire
  | Idle when limits.keepalive_timeout > 0 && elapsed > limits.keepalive_timeout
    ->
      Some Close_idle
  | Idle | Handshake | Headers | Request | Closing -> None

let cancel side =
  match side.pending with
  | Some cancel ->
      side.pending <- None;
      cancel ()
  | None -> ()

let expire t =
  let phase = t.phase in
  t.state <- Expired;
  (* [on_expiry] is given by the server; an exception from it must not stop the
     ticker, nor keep the pending operations from failing. *)
  (try t.on_expiry phase
   with exn ->
     Logs.err ~src:section (fun fmt ->
       fmt "Error while expiring a connection: %s" (Printexc.to_string exn)));
  cancel t.reads; cancel t.writes; cancel t.other

(* An idle connection is closed by failing its reads, the pending one and the
   later ones, with [Timed_out], so that the server stops waiting for a request.
   It enters [Closing], where the server can still end it properly, for
   instance with a TLS close_notify, within the timeout. *)
let close_idle t =
  t.state <- Closed t.reads;
  t.phase <- Closing;
  t.phase_since <- !now;
  cancel t.reads

let tick () =
  incr now;
  Hashtbl.fold
    (fun _ t acc ->
       match t.state with
       | Expired -> acc
       | Open | Closed _ -> (
         match action t with Some a -> (t, a) :: acc | None -> acc))
    guards []
  |> List.iter (fun (t, a) ->
    match a with Close_idle -> close_idle t | Expire -> expire t)

(* Whether operations on [side] must fail: all of them once the connection has
   expired, and those of the side the server closed. *)
let refused t side =
  match t.state with Open -> false | Closed s -> s == side | Expired -> true

let count_read t n =
  t.reads.bytes <- t.reads.bytes + n;
  match t.phase with
  | Idle when n > 0 -> set_phase t Headers
  | Idle | Handshake | Headers | Request | Closing -> ()

let count_write t n = t.writes.bytes <- t.writes.bytes + n
let count_nothing _ _ = ()

(* [p], an operation on [side] that has just started, watched: when it does not
   complete at once, it can be cancelled by the expiry of the connection, and
   the time it takes counts as waiting for the client. [count] records its
   result. *)
let watch t side ~count p =
  match Lwt.state p with
  | Lwt.Return x -> count t x; p
  | Lwt.Fail _ -> p
  | Lwt.Sleep ->
      side.since <- !now;
      side.pending <- Some (fun () -> Lwt.cancel p);
      let finish () =
        side.pending <- None;
        side.waited <- side.waited + (!now - side.since)
      in
      Lwt.try_bind
        (fun () -> p)
        (fun x ->
           finish ();
           (* An operation that could not be cancelled may still complete after
              the expiry, for instance with the end of file that follows the
              shutdown of the socket. It fails all the same, so that a
              truncated body is never taken for a complete one. *)
           if refused t side
           then Lwt.fail Timed_out
           else (count t x; Lwt.return x))
        (fun exn ->
           finish ();
           Lwt.fail (if refused t side then Timed_out else exn))

let read t f buf pos len =
  if refused t t.reads
  then Lwt.fail Timed_out
  else watch t t.reads ~count:count_read (f buf pos len)

let write t f buf pos len =
  if refused t t.writes
  then Lwt.fail Timed_out
  else watch t t.writes ~count:count_write (f buf pos len)

let run t f =
  if refused t t.other
  then Lwt.fail Timed_out
  else watch t t.other ~count:count_nothing (f ())

let started = ref false

let start () =
  if not !started
  then (
    started := true;
    let rec loop () = Lwt_unix.sleep 1. >>= fun () -> tick (); loop () in
    Lwt.async loop)

module Per_address = struct
  let counts : (Ipaddr.t, int) Hashtbl.t = Hashtbl.create 64

  let group = function
    | Ipaddr.V4 _ as ip -> ip
    | Ipaddr.V6 v6 -> (
      match Ipaddr.v4_of_v6 v6 with
      | Some v4 -> Ipaddr.V4 v4
      | None ->
          Ipaddr.V6 (Ipaddr.V6.Prefix.network (Ipaddr.V6.Prefix.make 64 v6)))

  let count ip = Option.value ~default:0 (Hashtbl.find_opt counts (group ip))

  let acquire ~limit ip =
    let group = group ip in
    let n = Option.value ~default:0 (Hashtbl.find_opt counts group) in
    if limit > 0 && n >= limit
    then false
    else (
      Hashtbl.replace counts group (n + 1);
      true)

  let release ip =
    let group = group ip in
    match Hashtbl.find_opt counts group with
    | Some n when n > 1 -> Hashtbl.replace counts group (n - 1)
    | Some _ | None -> Hashtbl.remove counts group
end
