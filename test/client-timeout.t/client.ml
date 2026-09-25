(* Client for the client timeout tests. Each scenario connects to the test
   server, whose timeout is one second, and prints what the server did.

   The only timing the output depends on is a generous bound: a connection
   counts as closed by the timeout when the server closes it between 0.5 and 5
   seconds after the timeout started to run. A read never waits more than 8
   seconds, so that a server that does not close shows as such instead of
   hanging the test. *)

type event = Data of string | Ended

type connection =
  { send : string -> unit
  ; receive : unit -> event
  ; readable_within : float -> bool
        (** whether something arrives, possibly the end of the connection,
            within this delay *) }

let connect port =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
  Unix.setsockopt_float fd Unix.SO_RCVTIMEO 8.;
  fd

let readable_within fd delay =
  match Unix.select [fd] [] [] delay with
  | [], _, _ -> false
  | _ :: _, _, _ -> true

(* A failed read is an ended connection: the server closed it (end of file,
   reset), or nothing came for 8 seconds, which the verdict then reports. *)
let receive_with read =
  let buffer = Bytes.create 4096 in
  fun () ->
    match read buffer with
    | 0 -> Ended
    | n -> Data (Bytes.sub_string buffer 0 n)
    | exception (Unix.Unix_error _ | Ssl.Read_error _) -> Ended

let plain port =
  let fd = connect port in
  { send = (fun s -> ignore (Unix.write_substring fd s 0 (String.length s)))
  ; receive =
      receive_with (fun buffer -> Unix.read fd buffer 0 (Bytes.length buffer))
  ; readable_within = readable_within fd }

let tls port =
  let fd = connect port in
  let socket =
    Ssl.embed_socket fd (Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context)
  in
  Ssl.connect socket;
  { send = Ssl.output_string socket
  ; receive =
      receive_with (fun buffer -> Ssl.read socket buffer 0 (Bytes.length buffer))
  ; readable_within = readable_within fd }

let request ?(meth = "GET") ?(headers = []) path =
  String.concat "\r\n"
    ((meth ^ " " ^ path ^ " HTTP/1.1") :: "Host: localhost" :: headers)
  ^ "\r\n\r\n"

let rec index_of sub s i =
  if i + String.length sub > String.length s
  then None
  else if String.equal (String.sub s i (String.length sub)) sub
  then Some i
  else index_of sub s (i + 1)

let content_length header_lines =
  List.find_map
    (fun line ->
       match String.index_opt line ':' with
       | Some i
         when String.equal
                (String.lowercase_ascii (String.sub line 0 i))
                "content-length" ->
           int_of_string_opt
             (String.trim
                (String.sub line (i + 1) (String.length line - i - 1)))
       | Some _ | None -> None)
    header_lines

(* The status line and the body of the response in [data], if it is complete.
   The test server always sends a content length. *)
let parse_response data =
  match index_of "\r\n\r\n" data 0 with
  | None -> None
  | Some i -> (
      let head =
        String.sub data 0 i |> String.split_on_char '\n'
        |> List.map String.trim
      in
      let body = String.sub data (i + 4) (String.length data - i - 4) in
      match head, content_length head with
      | status :: _, Some n when String.length body >= n ->
          Some (status, String.sub body 0 n)
      | _ -> None)

let print_response conn =
  let rec read_response data =
    match parse_response data with
    | Some (status, body) -> Printf.printf "%s: %s\n" status body
    | None -> (
      match conn.receive () with
      | Data d -> read_response (data ^ d)
      | Ended -> print_endline "connection ended before a complete response")
  in
  read_response ""

(* Waits for the server to close the connection, and reports when, and whether
   it answered anything before. *)
let await_close conn ~since =
  let rec drain received =
    match conn.receive () with
    | Data d -> drain (received ^ d)
    | Ended -> received
  in
  let received = drain "" in
  let elapsed = Unix.gettimeofday () -. since in
  Printf.printf "%s, %s\n"
    (if elapsed < 0.5
     then "closed too early"
     else if elapsed <= 5.
     then "closed by the server after the timeout"
     else "not closed by the server")
    (if String.equal received "" then "without answering" else "after answering")

(* Headers that never end. *)
let partial_headers conn =
  let since = Unix.gettimeofday () in
  conn.send "GET / HTTP/1.1\r\nHost: localhost\r\n";
  await_close conn ~since

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
  await_close conn ~since

(* A connection kept alive after a response, with nothing more sent. *)
let idle_keepalive conn =
  conn.send (request "/");
  print_response conn;
  await_close conn ~since:(Unix.gettimeofday ())

(* A response that takes longer than the timeout to be produced. *)
let slow_response conn =
  conn.send (request "/slow");
  print_response conn

(* A body of 12 bytes, one byte every 0.4 s: about 5 s in all, far beyond the
   timeout, but each byte comes within it. *)
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
  await_close conn ~since:(Unix.gettimeofday ())

let () =
  (* A write to a connection the server closed fails instead of killing us. *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Ssl.init ();
  let scenario, conn =
    match Sys.argv with
    | [|_; scenario|] -> scenario, plain 8076
    | [|_; scenario; "tls"|] -> scenario, tls 8454
    | _ -> failwith "usage: client scenario [tls]"
  in
  match scenario with
  | "partial-headers" -> partial_headers conn
  | "drip-headers" -> drip_headers conn
  | "idle-keepalive" -> idle_keepalive conn
  | "slow-response" -> slow_response conn
  | "slow-body" -> slow_body conn
  | "stalled-body" -> stalled_body conn
  | _ -> failwith ("unknown scenario " ^ scenario)
