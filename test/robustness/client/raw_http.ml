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

let is_status_line line = String.starts_with ~prefix:"HTTP/" line

let is_digits ~hex s =
  s <> ""
  && String.for_all
       (function
         | '0' .. '9' -> true | 'a' .. 'f' | 'A' .. 'F' -> hex | _ -> false)
       s

(* The status code of a status line, "HTTP/1.1 200 OK". [int_of_string_opt]
   alone would also accept "+20" or "0x1F4". *)
let status_code line =
  if is_status_line line
  then
    match String.split_on_char ' ' line with
    | _ :: code :: _ when String.length code = 3 && is_digits ~hex:false code ->
        int_of_string_opt code
    | _ -> None
  else None

type framing = Complete of {close : bool} | Incomplete | Unframed

let ( let* ) = Result.bind

let substring_at s pos sub =
  let rec loop i =
    i = String.length sub || (s.[pos + i] = sub.[i] && loop (i + 1))
  in
  pos + String.length sub <= String.length s && loop 0

let rec find s pos sub =
  if pos + String.length sub > String.length s
  then None
  else if substring_at s pos sub
  then Some pos
  else find s (pos + 1) sub

let line_end s pos =
  match find s pos "\r\n" with Some i -> Ok i | None -> Error Incomplete

(* Position after the trailer section that starts at [pos]. *)
let rec trailers_end s pos =
  if substring_at s pos "\r\n"
  then Ok (pos + 2)
  else
    let* eol = line_end s pos in
    trailers_end s (eol + 2)

(* Position after the chunked content that starts at [pos]. Lengths are
   compared by subtraction, as a huge one would overflow an addition. *)
let rec chunked_end s pos =
  let* eol = line_end s pos in
  let size = String.sub s pos (eol - pos) in
  let size =
    String.trim
      (match String.index_opt size ';' with
      | Some i -> String.sub size 0 i
      | None -> size)
  in
  if not (is_digits ~hex:true size)
  then Error Unframed
  else
    match int_of_string_opt ("0x" ^ size) with
    | Some 0 -> trailers_end s (eol + 2)
    | Some n when n > 0 ->
        if n > String.length s - eol - 4
        then Error Incomplete
        else if substring_at s (eol + 2 + n) "\r\n"
        then chunked_end s (eol + 4 + n)
        else Error Unframed
    | Some _ | None -> Error Unframed

let field_value name line =
  match String.index_opt line ':' with
  | Some i when String.lowercase_ascii (String.sub line 0 i) = name ->
      Some (String.trim (String.sub line (i + 1) (String.length line - i - 1)))
  | Some _ | None -> None

let list_elements value =
  List.map
    (fun element -> String.lowercase_ascii (String.trim element))
    (String.split_on_char ',' value)

let is_chunked coding =
  match List.rev (list_elements coding) with
  | last :: _ -> last = "chunked"
  | [] -> false

type response = {next : int; final : bool; close : bool}

(* The response that starts at [pos] (RFC 9112 section 6.3): the position
   after it, whether it is a final one (not 1xx), and whether it closes the
   connection. A response delimited by the end of the connection, or followed
   by another protocol (101), is [Unframed]. *)
let response_end s pos =
  let* head_end =
    match find s pos "\r\n\r\n" with Some i -> Ok i | None -> Error Incomplete
  in
  let content = head_end + 4 in
  match split_lines (String.sub s pos (head_end - pos)) with
  | [] -> Error Unframed
  | status :: fields -> (
      let values name = List.filter_map (field_value name) fields in
      let last name =
        match List.rev (values name) with
        | value :: _ -> Some value
        | [] -> None
      in
      let options = List.concat_map list_elements (values "connection") in
      (* An HTTP/1.0 response is not persistent unless it says so (RFC 9112
         section 9.3). *)
      let close =
        List.mem "close" options
        || String.starts_with ~prefix:"HTTP/1.0 " status
           && not (List.mem "keep-alive" options)
      in
      match status_code status with
      | None | Some 101 -> Error Unframed
      | Some code when code / 100 = 1 ->
          Ok {next = content; final = false; close}
      | Some (204 | 304) -> Ok {next = content; final = true; close}
      | Some _ -> (
        match last "transfer-encoding", last "content-length" with
        | Some coding, _ when is_chunked coding ->
            let* next = chunked_end s content in
            Ok {next; final = true; close}
        | Some _, _ | None, None -> Error Unframed
        | None, Some length -> (
          match
            if is_digits ~hex:false length
            then int_of_string_opt length
            else None
          with
          | Some n when n <= String.length s - content ->
              Ok {next = content + n; final = true; close}
          | Some _ -> Error Incomplete
          | None -> Error Unframed)))

(* The framing of the responses received from position [pos] on, [last]
   being [Some close] when they follow a final response. The state returned,
   [(pos', last')], is the one at the start of the first response that is not
   complete: scanning more bytes can resume from there. *)
let rec scan received pos last =
  if pos = String.length received
  then
    ( (match last with Some close -> Complete {close} | None -> Incomplete)
    , (pos, last) )
  else
    match response_end received pos with
    | Ok {next; final; close} ->
        scan received next (if final then Some close else None)
    | Error framing -> framing, (pos, last)

let framing received = fst (scan received 0 None)

let receive ?(idle = 2.) ?(settle = idle) ?(limit = 1 lsl 20) conn =
  let buffer = Buffer.create 4096 in
  let chunk = Bytes.create 65536 in
  (* The delay before the next read, [idle] seconds, or [settle] seconds after
     complete responses that leave the connection open: after one that closes
     it, the close is waited for. [state] is the one of [scan], with positions
     in [buffer]: the bytes of the complete responses are not scanned again. *)
  let delay ((start, last) as state) =
    if settle < idle
    then
      let framing, (next, last) =
        scan (Buffer.sub buffer start (Buffer.length buffer - start)) 0 last
      in
      ( (if framing = Complete {close = false} then settle else idle)
      , (start + next, last) )
    else idle, state
  in
  (* A zero timeout would mean "no timeout", hence the lower bound. *)
  let read delay =
    Unix.setsockopt_float conn.fd Unix.SO_RCVTIMEO (Float.max delay 0.001);
    match conn.channel with
    | Plain_fd -> Unix.read conn.fd chunk 0 (Bytes.length chunk)
    | Tls_socket socket -> Ssl.read socket chunk 0 (Bytes.length chunk)
  in
  let rec loop state =
    if Buffer.length buffer >= limit
    then Overflow
    else
      let delay, state = delay state in
      match read delay with
      | 0 -> Closed
      | n ->
          Buffer.add_subbytes buffer chunk 0 n;
          loop state
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop state
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
          Idle
      | exception Unix.Unix_error _ -> Reset
      | exception Ssl.Read_error error -> tls_read_ending error
  in
  let ending = loop (0, None) in
  {received = Buffer.contents buffer; ending}

let close conn = try Unix.close conn.fd with Unix.Unix_error _ -> ()

let exchange ?trickle ?idle ?settle ?limit target request =
  let conn = connect target in
  Fun.protect
    ~finally:(fun () -> close conn)
    (fun () ->
       let (_ : bool) = send ?trickle conn request in
       receive ?idle ?settle ?limit conn)

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

let status_codes received = List.filter_map status_code (split_lines received)
