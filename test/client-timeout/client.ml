(* Client for the client timeout tests. Each scenario connects to the test
   server (see server.ml for its bounds) and prints what the server did.

   The only timings the output depends on are generous: a connection counts as
   closed in time when the server closes it no earlier than half a second
   before its bound, and no later than 3.5 seconds after (the server checks its
   bounds once a second). A read never waits more than 10 seconds, so that a
   server that does not close shows as such instead of hanging the test. *)

type event = Data of string | Ended

type connection =
  { fd : Unix.file_descr
  ; send : string -> unit
  ; receive : unit -> event
  ; readable_within : float -> bool
    (** whether something arrives, possibly the end of the connection,
            within this delay *)
  }

let connect port =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
  Unix.setsockopt_float fd Unix.SO_RCVTIMEO 10.;
  fd

let readable_within fd delay =
  match Unix.select [fd] [] [] delay with
  | [], _, _ -> false
  | _ :: _, _, _ -> true

(* A failed read is an ended connection: the server closed it (end of file,
   reset, TLS close_notify), or nothing came for 10 seconds, which the verdict
   then reports. *)
let receive_with read =
  let buffer = Bytes.create 65536 in
  fun () ->
    match read buffer with
    | 0 -> Ended
    | n -> Data (Bytes.sub_string buffer 0 n)
    | exception (Unix.Unix_error _ | Ssl.Read_error _) -> Ended

(* A write to a connection the server closed is ignored: what the server did
   shows in what the client receives. *)
let send_with write s =
  try write s with Unix.Unix_error _ | Ssl.Write_error _ -> ()

let raw_receive fd =
  receive_with (fun buffer -> Unix.read fd buffer 0 (Bytes.length buffer))

let plain port =
  let fd = connect port in
  { fd
  ; send =
      send_with (fun s ->
        ignore (Unix.write_substring fd s 0 (String.length s)))
  ; receive = raw_receive fd
  ; readable_within = readable_within fd }

let tls port =
  let fd = connect port in
  let socket =
    Ssl.embed_socket fd (Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context)
  in
  Ssl.connect socket;
  { fd
  ; send = send_with (Ssl.output_string socket)
  ; receive =
      receive_with (fun buffer ->
        Ssl.read socket buffer 0 (Bytes.length buffer))
  ; readable_within = readable_within fd }

let request ?(meth = "GET") ?(version = "1.1") ?(headers = []) path =
  String.concat "\r\n"
    ((meth ^ " " ^ path ^ " HTTP/" ^ version) :: "Host: localhost" :: headers)
  ^ "\r\n\r\n"

let rec index_of sub s i =
  if i + String.length sub > String.length s
  then None
  else if String.equal (String.sub s i (String.length sub)) sub
  then Some i
  else index_of sub s (i + 1)

let header_value name lines =
  List.find_map
    (fun line ->
       match String.index_opt line ':' with
       | Some i when String.lowercase_ascii (String.sub line 0 i) = name ->
           Some
             (String.trim
                (String.sub line (i + 1) (String.length line - i - 1)))
       | Some _ | None -> None)
    lines

let content_length header_lines =
  Option.bind (header_value "content-length" header_lines) int_of_string_opt

(* The status line and the body of the response in [data], if it is complete.
   The test server always sends a content length. *)
let parse_response data =
  match index_of "\r\n\r\n" data 0 with
  | None -> None
  | Some i -> (
      let head =
        String.sub data 0 i |> String.split_on_char '\n' |> List.map String.trim
      in
      let body = String.sub data (i + 4) (String.length data - i - 4) in
      match head, content_length head with
      | status :: _, Some n when String.length body >= n ->
          Some (status, String.sub body 0 n)
      | _ -> None)

(* The status line and the body of the next response on [conn]. *)
let response conn =
  let rec read_response data =
    match parse_response data with
    | Some (status, body) -> Printf.sprintf "%s: %s" status body
    | None -> (
      match conn.receive () with
      | Data d -> read_response (data ^ d)
      | Ended -> "connection ended before a complete response")
  in
  read_response ""

let print_response conn = print_endline (response conn)

let rec drain conn received =
  match conn.receive () with
  | Data d -> drain conn (received + String.length d)
  | Ended -> received

type timing = Too_early | In_time | Too_late

(* How something the server did [elapsed] seconds after a client started to
   wait compares with a bound of [bound] seconds. *)
let timing ~bound elapsed =
  if elapsed < bound -. 0.5
  then Too_early
  else if elapsed <= bound +. 3.5
  then In_time
  else Too_late

(* Waits for the server to close the connection, and reports whether it did so
   in time, [bound] seconds after [since], and whether it answered anything
   before. *)
let await_close ?(what = "the timeout") ~bound conn ~since =
  let received = drain conn 0 in
  Printf.printf "%s, %s\n"
    (match timing ~bound (Unix.gettimeofday () -. since) with
    | Too_early -> "closed too early"
    | In_time -> "closed by the server after " ^ what
    | Too_late -> "not closed by the server")
    (if received = 0 then "without answering" else "after answering")

let timeout = 2.
let keepalive_timeout = 3.

(* Headers that never end. *)
let partial_headers conn =
  let since = Unix.gettimeofday () in
  conn.send "GET / HTTP/1.1\r\nHost: localhost\r\n";
  await_close conn ~bound:timeout ~since

(* The request, one byte every 0.3 s: each byte comes well within the timeout,
   but the whole request would take about 10 s. *)
let drip_headers conn =
  let since = Unix.gettimeofday () in
  let req = request "/" in
  let rec drip i =
    if i < String.length req && not (conn.readable_within 0.3)
    then (
      conn.send (String.make 1 req.[i]);
      drip (i + 1))
    else i
  in
  if drip 0 = String.length req then print_endline "the whole request was sent";
  await_close conn ~bound:timeout ~since

(* A connection kept alive after a response, with nothing more sent. *)
let idle_keepalive conn =
  conn.send (request "/");
  print_response conn;
  await_close conn ~what:"the keep-alive timeout" ~bound:keepalive_timeout
    ~since:(Unix.gettimeofday ())

(* Three requests on one connection, 2.5 s apart: each pause is within the
   keep-alive timeout, the whole is not. *)
let keepalive_reuse conn =
  for i = 1 to 3 do
    if i > 1 then Unix.sleepf 2.5;
    conn.send (request "/");
    print_response conn
  done

(* A response that takes longer than the timeout to be produced. *)
let slow_response conn =
  conn.send (request "/slow");
  print_response conn

(* A body of 12 bytes, one byte every 0.4 s: about 5 s in all, far beyond the
   timeout, but each byte comes within it, at 2.5 bytes per second, above the
   minimum rate. *)
let slow_body conn =
  let body = "abcdefghijkl" in
  conn.send
    (request ~meth:"POST"
       ~headers:[Printf.sprintf "Content-Length: %d" (String.length body)]
       "/length");
  String.iter
    (fun c ->
       Unix.sleepf 0.4;
       conn.send (String.make 1 c))
    body;
  print_response conn

(* A body that stops after 3 of its 10 bytes. *)
let stalled_body conn =
  conn.send
    (request ~meth:"POST" ~headers:["Content-Length: 10"] "/length" ^ "abc");
  await_close conn ~bound:timeout ~since:(Unix.gettimeofday ())

(* A body of 100 bytes, one byte every 1.5 s: each byte comes within the
   timeout, but at 0.67 bytes per second, below the minimum rate ("R-U-Dead-Yet"
   attack). The whole body would take 150 s. *)
let drip_body conn =
  conn.send (request ~meth:"POST" ~headers:["Content-Length: 100"] "/length");
  let since = Unix.gettimeofday () in
  let rec drip i =
    if i < 100 && not (conn.readable_within 1.5)
    then (
      conn.send "a";
      drip (i + 1))
    else i
  in
  let sent = drip 0 in
  let received = drain conn 0 in
  let elapsed = Unix.gettimeofday () -. since in
  Printf.printf "%s, %s\n"
    (if sent = 100
     then "the whole body was sent"
     else if elapsed <= 10.
     then "closed by the server before the end of the body"
     else "closed too late")
    (if received = 0 then "without answering" else "after answering")

(* A client that asks for a response larger than the socket buffers and stops
   reading it. After 6 s, it reads what it can. *)
let slow_reader conn =
  conn.send (request "/big");
  Unix.sleepf 6.;
  let received = drain conn 0 in
  print_endline
    (if received < 32 * 1024 * 1024
     then "the server cut the response"
     else "the whole response arrived")

(* Five connections at once from the same address, for a limit of four. *)
let per_address port =
  let open_ = List.init 4 (fun _ -> connect port) in
  let fifth = plain port in
  let since = Unix.gettimeofday () in
  let refused =
    fifth.readable_within 1.
    && fifth.receive () = Ended
    && Unix.gettimeofday () -. since < 1.
  in
  print_endline
    (if refused
     then "the fifth connection is refused at once"
     else "the fifth connection is accepted");
  List.iter Unix.close (fifth.fd :: open_);
  Unix.sleepf 0.5;
  let again = plain port in
  again.send (request "/");
  print_string "once they are closed: ";
  print_response again

(* A TLS connection kept alive after a response, then left idle, by a client
   that never answers the close_notify the server sends at the keep-alive
   timeout. The connection is read raw from then on, to see when the
   close_notify comes, and when the server closes its socket. *)
let mute conn =
  conn.send (request "/");
  print_response conn;
  let since = Unix.gettimeofday () in
  let raw = {conn with receive = raw_receive conn.fd} in
  match raw.receive () with
  | Ended -> print_endline "closed without a close_notify"
  | Data _ ->
      let notified = Unix.gettimeofday () in
      ignore (drain raw 0 : int);
      Printf.printf "close_notify %s, then the socket closed %s\n"
        (match timing ~bound:keepalive_timeout (notified -. since) with
        | Too_early -> "too early"
        | In_time -> "after the keep-alive timeout"
        | Too_late -> "too late")
        (if Unix.gettimeofday () -. notified < 1. then "at once" else "late")

(* A TLS client that leaves without ending its session: it closes its socket
   once it has its response, without a close_notify. *)
let abrupt conn =
  conn.send (request "/");
  print_response conn;
  Unix.close conn.fd

(* The pool server is full at 4 connections (see server.ml). Four connections
   that their client does not use, opened by [open_unused], then a fifth,
   opened by [open_fifth]: the fifth is answered, the server having closed the
   oldest unused connections to make room, one when the fourth came, one when
   the fifth did. All five must be on the same port: the server accepts the
   connections to a port in order, but not those to different ports, so it
   could take the fifth first, then close it, answered and idle, as the oldest
   unused one. *)
let make_room ~open_fifth open_unused =
  let unused = List.init 4 (fun _ -> open_unused ()) in
  let fifth = open_fifth () in
  fifth.send (request "/");
  Printf.printf "a fifth connection: %s\n" (response fifth);
  let closed =
    List.mapi (fun i conn -> i + 1, conn) unused
    |> List.filter_map (fun (i, conn) ->
      if conn.readable_within 0.5 && conn.receive () = Ended
      then Some (string_of_int i)
      else None)
  in
  Printf.printf "closed to make room: %s\n" (String.concat " " closed);
  List.iter (fun conn -> Unix.close conn.fd) (fifth :: unused)

(* Four busy connections on the pool server, then a fifth: it is answered once
   a busy connection has answered, the server then closing it, idle, to make
   room. The busy connections come 0.2 s apart, so that the server has their
   requests before the next one comes, and does not take them for unused. *)
let pool_busy port =
  let since = Unix.gettimeofday () in
  let busy =
    List.init 4 (fun _ ->
      let conn = plain port in
      conn.send (request "/slow");
      Unix.sleepf 0.2;
      conn)
  in
  let fifth = plain port in
  fifth.send (request "/");
  let answer = response fifth in
  Printf.printf "a fifth connection, %s: %s\n"
    (if Unix.gettimeofday () -. since >= 3.5
     then "once a busy one was done"
     else "at once")
    answer;
  Printf.printf "busy connections answered: %d\n"
    (List.length
       (List.filter
          (fun conn -> response conn = "HTTP/1.1 200 OK: slow response")
          busy));
  List.iter (fun conn -> Unix.close conn.fd) (fifth :: busy)

let rec occurrences sub s i =
  match index_of sub s i with
  | Some j -> 1 + occurrences sub s (j + 1)
  | None -> 0

(* Requests, sent at once, after which the connection is, or is not, to be
   kept alive: prints the status and the Connection header of the first
   response, the number of responses, and whether the server closes the
   connection at once. *)
let keep_alive requests conn =
  conn.send (String.concat "" requests);
  (* What the server sends until it closes the connection, or until nothing
     comes for a second, and whether it closed it. *)
  let rec receive data =
    if conn.readable_within 1.
    then
      match conn.receive () with
      | Data d -> receive (data ^ d)
      | Ended -> data, true
    else data, false
  in
  let data, closed = receive "" in
  let lines = String.split_on_char '\n' data |> List.map String.trim in
  Printf.printf "%s, Connection: %s, responses: %d, %s\n"
    (match lines with
    | status :: _ when status <> "" -> status
    | _ -> "no response")
    (Option.value ~default:"none" (header_value "connection" lines))
    (occurrences "HTTP/1.1 " data 0)
    (if closed then "closed by the server at once" else "kept open")

let () =
  (* A write to a connection the server closed fails instead of killing us. *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Ssl.init ();
  let plain_port = 8076 and tls_port = 8454 in
  let limit_port = 8077 in
  let pool_port = 8078 and pool_tls_port = 8455 in
  match Sys.argv with
  | [|_; "per-address"|] -> per_address limit_port
  | [|_; "pool-idle"|] ->
      make_room
        ~open_fifth:(fun () -> plain pool_port)
        (fun () ->
           let conn = plain pool_port in
           conn.send (request "/");
           ignore (response conn : string);
           conn)
  | [|_; "pool-silent"|] ->
      make_room
        ~open_fifth:(fun () -> plain pool_port)
        (fun () -> plain pool_port)
  | [|_; "pool-handshake"|] ->
      (* TCP connections to the TLS port, on which nothing is sent, then a
         TLS client. *)
      make_room
        ~open_fifth:(fun () -> tls pool_tls_port)
        (fun () -> plain pool_tls_port)
  | [|_; "pool-busy"|] -> pool_busy pool_port
  | [|_; "no-handshake"|] ->
      (* A TCP connection to the TLS port, on which nothing is sent. *)
      await_close (plain tls_port) ~bound:timeout ~since:(Unix.gettimeofday ())
  | [|_; "mute"|] -> mute (tls tls_port)
  | [|_; "abrupt"|] -> abrupt (tls tls_port)
  | [|_; "connection-close"|] ->
      keep_alive [request ~headers:["Connection: close"] "/"] (plain plain_port)
  | [|_; "pipelined-after-close"|] ->
      keep_alive
        [request ~headers:["Connection: close"] "/"; request "/"]
        (plain plain_port)
  | [|_; "close-among-options"|] ->
      keep_alive
        [request ~headers:["Connection: TE, Close"; "TE: trailers"] "/"]
        (plain plain_port)
  | [|_; "keep-alive-among-options"|] ->
      keep_alive
        [request ~headers:["Connection: Keep-Alive, TE"; "TE: trailers"] "/"]
        (plain plain_port)
  | [|_; "http-1.0"|] ->
      keep_alive [request ~version:"1.0" "/"] (plain plain_port)
  | [|_; "http-1.0-keep-alive"|] ->
      keep_alive
        [request ~version:"1.0" ~headers:["Connection: Keep-Alive"] "/"]
        (plain plain_port)
  | [|_; scenario|] | [|_; scenario; "tls"|] -> (
      let conn =
        if Array.length Sys.argv = 3 then tls tls_port else plain plain_port
      in
      match scenario with
      | "partial-headers" -> partial_headers conn
      | "drip-headers" -> drip_headers conn
      | "idle-keepalive" -> idle_keepalive conn
      | "keepalive-reuse" -> keepalive_reuse conn
      | "slow-response" -> slow_response conn
      | "slow-body" -> slow_body conn
      | "stalled-body" -> stalled_body conn
      | "drip-body" -> drip_body conn
      | "slow-reader" -> slow_reader conn
      | _ -> failwith ("unknown scenario " ^ scenario))
  | _ -> failwith "usage: client scenario [tls]"
