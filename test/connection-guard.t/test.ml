(* Unit tests of Connection_guard. The clock is driven by calling [tick], and
   the reads and writes of a connection are promises resolved by hand, so that
   nothing depends on real time. *)

module G = Ocsigen.Connection_guard

let limits ?(timeout = 3) ?(keepalive_timeout = 5) ?(min_rate = 0) () =
  {G.timeout; keepalive_timeout; min_rate}

let phase_name = function
  | G.Handshake -> "Handshake"
  | G.Headers -> "Headers"
  | G.Idle -> "Idle"
  | G.Request -> "Request"
  | G.Closing -> "Closing"

(* A guard that records the phase it expired in. *)
let guard ?limits:(l = limits ()) phase =
  let expired_in = ref None in
  let t = G.create l phase ~on_expiry:(fun p -> expired_in := Some p) in
  t, expired_in

let buffer = Lwt_bytes.create 16

(* A read that stays pending until its resolver is woken up. *)
let pending_read t =
  let p, u = Lwt.task () in
  G.read t (fun _ _ _ -> p) buffer 0 16, u

let pending_write t =
  let p, u = Lwt.task () in
  G.write t (fun _ _ _ -> p) buffer 0 16, u

let complete_read t n = ignore (G.read t (fun _ _ _ -> Lwt.return n) buffer 0 16)

let outcome p =
  match Lwt.state p with
  | Lwt.Return n -> Printf.sprintf "returned %d" n
  | Lwt.Fail G.Timed_out -> "failed with Timed_out"
  | Lwt.Fail exn -> "failed with " ^ Printexc.to_string exn
  | Lwt.Sleep -> "pending"

(* Ticks until [t] expires, at most [max] times, running [each] before each
   tick, and reports when and in which phase it expired. *)
let run_until_expiry ?(max = 100) ?(each = fun _ -> ()) (t, expired_in) =
  let rec go i =
    if i > max
    then Printf.printf "not expired after %d ticks\n" max
    else (
      each i;
      G.tick ();
      match !expired_in with
      | Some p -> Printf.printf "expired after %d ticks, in %s\n" i (phase_name p)
      | None ->
          if G.expired t
          then print_endline "expired without calling on_expiry"
          else go (i + 1))
  in
  go 1;
  G.release t

let section title = Printf.printf "\n%s\n" title

let () =
  print_string "Headers, timeout 3: ";
  run_until_expiry (guard G.Headers);
  print_string "Headers, one byte read each tick: ";
  let ((t, _) as g) = guard G.Headers in
  run_until_expiry g ~each:(fun _ -> complete_read t 1);
  print_string "Headers, timeout 0: ";
  run_until_expiry (guard ~limits:(limits ~timeout:0 ()) G.Headers);
  print_string "Handshake, timeout 3: ";
  run_until_expiry (guard G.Handshake);
  print_string "Closing, timeout 3: ";
  run_until_expiry (guard G.Closing)

let () =
  section "Idle";
  (* Past the keep-alive timeout, an idle connection is closed, not expired:
     its reads fail, and it enters Closing, bounded by the timeout. *)
  let t, expired_in = guard G.Idle in
  let read, _ = pending_read t in
  let ticks = ref 0 in
  while G.phase t = G.Idle do
    G.tick (); incr ticks
  done;
  Printf.printf
    "keep-alive timeout 5: closed after %d ticks, the read %s, expired: %b, \
     on_expiry called: %b\n"
    !ticks (outcome read) (G.expired t) (!expired_in <> None);
  Printf.printf "a new read %s, a write %s\n"
    (outcome (fst (pending_read t)))
    (outcome (G.write t (fun _ _ _ -> Lwt.return 16) buffer 0 16));
  G.set_phase t G.Idle;
  Printf.printf "the phase stays %s; " (phase_name (G.phase t));
  run_until_expiry (t, expired_in);
  print_string "the same, never ended: ";
  run_until_expiry (guard G.Idle);
  print_string "keep-alive timeout 0: ";
  run_until_expiry (guard ~limits:(limits ~keepalive_timeout:0 ()) G.Idle);
  print_string "a byte after 4 ticks starts the headers: ";
  let ((t, _) as g) = guard G.Idle in
  run_until_expiry g ~each:(fun i ->
    if i = 5
    then (
      complete_read t 1;
      Printf.printf "(now %s) " (phase_name (G.phase t))));
  print_string "an empty read does not start the headers: ";
  let ((t, _) as g) = guard G.Idle in
  run_until_expiry g ~each:(fun i -> if i = 5 then complete_read t 0)

let () =
  section "Request";
  print_string "no read pending (slow handler): ";
  run_until_expiry (guard G.Request);
  print_string "a read stalled from tick 10: ";
  let ((t, _) as g) = guard G.Request in
  let read = ref None in
  run_until_expiry g ~each:(fun i ->
    if i = 10 then read := Some (fst (pending_read t)));
  Option.iter (fun r -> Printf.printf "the read %s\n" (outcome r)) !read;
  print_string "one byte every 2 ticks, no minimum rate: ";
  let slow = ref None in
  let drip t i =
    if i mod 2 = 1
    then (
      Option.iter (fun u -> Lwt.wakeup u 1) !slow;
      slow := Some (snd (pending_read t)))
  in
  let ((t, _) as g) = guard G.Request in
  run_until_expiry g ~each:(drip t);
  print_string "one byte every 2 ticks, minimum rate 500 B/s: ";
  slow := None;
  let ((t, _) as g) = guard ~limits:(limits ~min_rate:500 ()) G.Request in
  run_until_expiry g ~each:(drip t);
  print_string "the same, timeout 0: ";
  slow := None;
  let ((t, _) as g) =
    guard ~limits:(limits ~timeout:0 ~min_rate:500 ()) G.Request
  in
  run_until_expiry g ~each:(drip t);
  print_string "1000 bytes a tick, minimum rate 500 B/s: ";
  let fast = ref None in
  let ((t, _) as g) = guard ~limits:(limits ~min_rate:500 ()) G.Request in
  run_until_expiry g ~each:(fun _ ->
    Option.iter (fun u -> Lwt.wakeup u 1000) !fast;
    fast := Some (snd (pending_read t)));
  (* 10 ticks at 1000 bytes a tick earn 20 seconds of credit at 500 B/s, which
     the next request does not inherit. *)
  let fast_then_drip ~new_request =
    let ((t, _) as g) = guard ~limits:(limits ~min_rate:500 ()) G.Request in
    let fast = ref None in
    slow := None;
    run_until_expiry g ~each:(fun i ->
      if i <= 10
      then (
        Option.iter (fun u -> Lwt.wakeup u 1000) !fast;
        fast := Some (snd (pending_read t)))
      else (
        if i = 11
        then (
          Option.iter (fun u -> Lwt.wakeup u 1000) !fast;
          if new_request then G.set_phase t G.Request);
        drip t i))
  in
  print_string "a fast body, then one byte every 2 ticks: ";
  fast_then_drip ~new_request:false;
  print_string "the same, the drip being a new request: ";
  fast_then_drip ~new_request:true

let () =
  section "Writes, in any phase";
  print_string "a write stalled while Idle (no keep-alive bound): ";
  let ((t, _) as g) =
    guard ~limits:(limits ~keepalive_timeout:0 ()) G.Idle
  in
  let write = ref None in
  run_until_expiry g ~each:(fun i ->
    if i = 1 then write := Some (fst (pending_write t)));
  Option.iter (fun w -> Printf.printf "the write %s\n" (outcome w)) !write;
  print_string "a client reading 1 byte every 2 ticks, minimum rate 500 B/s: ";
  let slow = ref None in
  let ((t, _) as g) = guard ~limits:(limits ~min_rate:500 ()) G.Request in
  run_until_expiry g ~each:(fun i ->
    if i mod 2 = 1
    then (
      Option.iter (fun u -> Lwt.wakeup u 1) !slow;
      slow := Some (snd (pending_write t))));
  print_string "a write stalled, timeout 0, minimum rate 500 B/s: ";
  let ((t, _) as g) =
    guard ~limits:(limits ~timeout:0 ~min_rate:500 ()) G.Request
  in
  run_until_expiry g ~each:(fun i -> if i = 1 then ignore (pending_write t))

let () =
  section "Closing after a response";
  let ((t, expired_in) as g) = guard G.Request in
  let read, _ = pending_read t in
  G.close t;
  Printf.printf
    "the pending read %s, expired: %b, on_expiry called: %b, phase: %s\n"
    (outcome read) (G.expired t) (!expired_in <> None) (phase_name (G.phase t));
  Printf.printf "a new read %s, a write %s\n"
    (outcome (fst (pending_read t)))
    (outcome (G.write t (fun _ _ _ -> Lwt.return 16) buffer 0 16));
  print_string "never ended: ";
  run_until_expiry g

let () =
  section "After the expiry";
  let t, _ = guard G.Headers in
  for _ = 1 to 4 do
    G.tick ()
  done;
  Printf.printf "expired: %b\n" (G.expired t);
  Printf.printf "a new read %s\n" (outcome (fst (pending_read t)));
  G.set_phase t G.Request;
  Printf.printf "the phase stays %s\n" (phase_name (G.phase t));
  G.release t;
  (* A promise made with [Lwt.wait] cannot be cancelled: its result comes after
     the expiry, as the end of file that follows the shutdown of a socket. *)
  let t, _ = guard G.Request in
  let p, u = Lwt.wait () in
  let read = G.read t (fun _ _ _ -> p) buffer 0 16 in
  for _ = 1 to 4 do
    G.tick ()
  done;
  Lwt.wakeup u 0;
  Printf.printf "a read completing after the expiry %s\n" (outcome read);
  G.release t;
  let t, _ = guard G.Handshake in
  let p, _ = Lwt.task () in
  let handshake = G.run t (fun () -> p) in
  for _ = 1 to 4 do
    G.tick ()
  done;
  Printf.printf "a handshake still pending at the expiry %s\n"
    (outcome handshake);
  G.release t;
  print_string "an exception from on_expiry: ";
  let t = G.create (limits ()) G.Headers ~on_expiry:(fun _ -> failwith "oops") in
  let read, _ = pending_read t in
  for _ = 1 to 4 do
    G.tick ()
  done;
  Printf.printf "the read %s\n" (outcome read);
  G.release t

(* With a capacity of 4, the connection that fills the server makes room by
   closing the unused connection that has waited the longest. *)
let () =
  section "Making room, capacity 4";
  G.set_capacity 4;
  let show name t = Printf.printf "%s: %s\n" name (phase_name (G.phase t)) in
  (* A connection that has received the start of a request. *)
  let talking () =
    let t, _ = guard G.Headers in
    complete_read t 5; t
  in
  let idle, _ = guard G.Idle in
  let idle_read, _ = pending_read idle in
  G.tick ();
  let busy, _ = guard G.Request in
  let silent, _ = guard G.Headers in
  let silent_read, _ = pending_read silent in
  print_endline "idle, busy and silent connections, then a fourth:";
  let fourth = talking () in
  show "the idle one" idle;
  Printf.printf "its read %s\n" (outcome idle_read);
  show "the busy one" busy;
  show "the silent one" silent;
  G.release idle;
  print_endline "a connection in its TLS handshake comes:";
  let handshake, _ = guard G.Handshake in
  let pending_handshake = G.run handshake (fun () -> fst (Lwt.task ())) in
  show "the silent one" silent;
  Printf.printf "its read %s\n" (outcome silent_read);
  show "the fourth" fourth;
  G.release silent;
  print_endline "another comes:";
  let sixth = talking () in
  show "the handshake" handshake;
  Printf.printf "the handshake %s\n" (outcome pending_handshake);
  G.release handshake;
  print_endline "another comes, all the others being busy:";
  let seventh = talking () in
  List.iter
    (fun (name, t) -> show name t)
    ["the busy one", busy; "the fourth", fourth; "the sixth", sixth];
  print_endline "the server is full; the fourth becomes idle, and a tick passes:";
  G.set_phase fourth G.Idle;
  G.tick ();
  show "the fourth" fourth;
  show "the seventh" seventh;
  List.iter G.release [busy; fourth; sixth; seventh];
  G.set_capacity max_int

let () =
  section "Connections per address";
  let ip = Ipaddr.of_string_exn in
  let acquire s =
    Printf.printf "acquire %s: %b\n" s
      (G.Per_address.acquire ~limit:2 (ip s))
  in
  acquire "192.0.2.1";
  acquire "192.0.2.1";
  acquire "192.0.2.1";
  acquire "192.0.2.2";
  G.Per_address.release (ip "192.0.2.1");
  acquire "192.0.2.1";
  acquire "::ffff:192.0.2.2";
  acquire "::ffff:192.0.2.2";
  acquire "2001:db8:1:2::1";
  acquire "2001:db8:1:2:ffff::7";
  acquire "2001:db8:1:2:abcd::1";
  acquire "2001:db8:1:3::1";
  Printf.printf "count 2001:db8:1:2::42: %d\n"
    (G.Per_address.count (ip "2001:db8:1:2::42"));
  List.iter
    (fun s -> G.Per_address.release (ip s))
    ["192.0.2.1"; "192.0.2.1"; "192.0.2.2"; "192.0.2.2"];
  Printf.printf "count 192.0.2.1 after its releases: %d\n"
    (G.Per_address.count (ip "192.0.2.1"));
  Printf.printf "no limit: %b\n"
    (List.for_all
       (fun _ -> G.Per_address.acquire ~limit:0 (ip "198.51.100.1"))
       (List.init 1000 Fun.id))
