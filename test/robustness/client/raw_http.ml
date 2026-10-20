type tls_version = Tls_1_2 | Tls_1_3

type transport =
  | Plain
  | Tls of {max_version : tls_version; sni : string option}

type target = {host : string; port : int; transport : transport}
type channel = Plain_fd | Tls_socket of Ssl.socket
type conn = {fd : Unix.file_descr; channel : channel}

exception Connect_failed of string

(* A failed write must be reported as an error, not kill the process. *)
let initialised =
  lazy
    (Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
     Ssl.init ())

(* Upper bound on any single blocking operation, so that a server that
   stops answering in the middle of a TLS record or handshake cannot hang a
   test. *)
let operation_timeout = 10.
let ssl_protocol = function Tls_1_2 -> Ssl.TLSv1_2 | Tls_1_3 -> Ssl.TLSv1_3

let tls_handshake fd ~max_version ~sni =
  let context = Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context in
  Ssl.set_min_protocol_version context Ssl.TLSv1_2;
  Ssl.set_max_protocol_version context (ssl_protocol max_version);
  let socket = Ssl.embed_socket fd context in
  Option.iter (Ssl.set_client_SNI_hostname socket) sni;
  match Ssl.connect socket with
  | () -> socket
  | exception Ssl.Connection_error _ ->
      raise (Connect_failed "TLS handshake failed")

let connect target =
  Lazy.force initialised;
  let address =
    match
      Unix.getaddrinfo target.host
        (string_of_int target.port)
        [Unix.AI_SOCKTYPE Unix.SOCK_STREAM]
    with
    | info :: _ -> info
    | [] -> raise (Connect_failed ("unknown host " ^ target.host))
  in
  let fd = Unix.socket address.ai_family Unix.SOCK_STREAM 0 in
  match
    Unix.setsockopt_float fd Unix.SO_RCVTIMEO operation_timeout;
    Unix.setsockopt_float fd Unix.SO_SNDTIMEO operation_timeout;
    Unix.connect fd address.ai_addr;
    match target.transport with
    | Plain -> Plain_fd
    | Tls {max_version; sni} -> Tls_socket (tls_handshake fd ~max_version ~sni)
  with
  | channel -> {fd; channel}
  | exception Unix.Unix_error (error, _, _) ->
      Unix.close fd;
      raise (Connect_failed (Unix.error_message error))
  | exception (Connect_failed _ as e) -> Unix.close fd; raise e

let write_all conn data =
  let length = String.length data in
  let write offset =
    match conn.channel with
    | Plain_fd -> Unix.write_substring conn.fd data offset (length - offset)
    | Tls_socket socket ->
        Ssl.write_substring socket data offset (length - offset)
  in
  let rec loop offset = if offset < length then loop (offset + write offset) in
  loop 0

let rec pieces size data =
  if String.length data <= size
  then [data]
  else
    String.sub data 0 size
    :: pieces size (String.sub data size (String.length data - size))

let send ?trickle conn data =
  match
    match trickle with
    | None -> write_all conn data
    | Some (size, _) when size <= 0 ->
        invalid_arg "Raw_http.send: trickle size must be positive"
    | Some (size, delay) ->
        List.iteri
          (fun i piece ->
             if i > 0 then Unix.sleepf delay;
             write_all conn piece)
          (pieces size data)
  with
  | () -> true
  | exception
      Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET | Unix.EAGAIN), _, _) ->
      false
  | exception Ssl.Write_error _ -> false

type ending = Closed | Reset | Idle | Overflow
type outcome = {received : string; ending : ending}

let unexpected_eof = Re.compile (Re.no_case (Re.str "unexpected eof"))

(* A read that times out (see [SO_RCVTIMEO] in [receive]) is reported as
   [Error_want_read]. OpenSSL reports a TCP close without TLS close_notify as
   an "unexpected eof" error; for the purpose of these tests it is a close. *)
let tls_read_ending = function
  | Ssl.Error_zero_return -> Closed
  | Ssl.Error_want_read -> Idle
  | Ssl.Error_ssl -> (
    match (Ssl.Error.get_error ()).reason with
    | Some reason when Re.execp unexpected_eof reason -> Closed
    | Some _ | None -> Reset)
  | _ -> Reset

let receive ?(idle = 2.) ?(limit = 1 lsl 20) conn =
  let buffer = Buffer.create 4096 in
  let chunk = Bytes.create 65536 in
  (* Each read blocks at most [idle] seconds. A zero timeout would mean "no
     timeout", hence the lower bound. *)
  Unix.setsockopt_float conn.fd Unix.SO_RCVTIMEO (Float.max idle 0.001);
  let read () =
    match conn.channel with
    | Plain_fd -> Unix.read conn.fd chunk 0 (Bytes.length chunk)
    | Tls_socket socket -> Ssl.read socket chunk 0 (Bytes.length chunk)
  in
  let rec loop () =
    if Buffer.length buffer >= limit
    then Overflow
    else
      match read () with
      | 0 -> Closed
      | n ->
          Buffer.add_subbytes buffer chunk 0 n;
          loop ()
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
          Idle
      | exception Unix.Unix_error _ -> Reset
      | exception Ssl.Read_error error -> tls_read_ending error
  in
  let ending = loop () in
  {received = Buffer.contents buffer; ending}

let close conn = try Unix.close conn.fd with Unix.Unix_error _ -> ()

let exchange ?trickle ?idle ?limit target request =
  let conn = connect target in
  Fun.protect
    ~finally:(fun () -> close conn)
    (fun () ->
       let (_ : bool) = send ?trickle conn request in
       receive ?idle ?limit conn)

let split_lines received =
  let lines = String.split_on_char '\n' received in
  let lines =
    (* A final line end leaves an empty last element, which is not a line. *)
    match List.rev lines with
    | "" :: rest -> List.rev rest
    | _ -> lines
  in
  List.map
    (fun line ->
       let n = String.length line in
       if n > 0 && line.[n - 1] = '\r' then String.sub line 0 (n - 1) else line)
    lines

let escape line =
  let buffer = Buffer.create (String.length line) in
  String.iter
    (function
      | ('\x20' .. '\x7e' | '\t') as c -> Buffer.add_char buffer c
      | c -> Printf.bprintf buffer "\\x%02X" (Char.code c))
    line;
  Buffer.contents buffer

let header_line names =
  Re.compile
    (Re.seq
       [ Re.bos
       ; Re.no_case (Re.alt (List.map (fun name -> Re.str (name ^ ":")) names))
       ])

let date_header = header_line ["date"]
let validator_header = header_line ["etag"; "last-modified"]
let is_status_line line = String.starts_with ~prefix:"HTTP/" line

let ending_line = function
  | Closed -> "[closed]"
  | Reset -> "[reset]"
  | Idle -> "[idle]"
  | Overflow -> "[overflow]"

let render ?(status_only = false) ?(validators = false) {received; ending} =
  let keep line =
    (not (Re.execp date_header line))
    && (validators || not (Re.execp validator_header line))
    && ((not status_only) || is_status_line line)
  in
  let lines = List.filter keep (split_lines received) in
  String.concat "" (List.map (fun line -> escape line ^ "\n") lines)
  ^ ending_line ending ^ "\n"

let status_codes received =
  List.filter_map
    (fun line ->
       if is_status_line line
       then
         match String.split_on_char ' ' line with
         | _ :: code :: _ -> int_of_string_opt code
         | _ -> None
       else None)
    (split_lines received)
