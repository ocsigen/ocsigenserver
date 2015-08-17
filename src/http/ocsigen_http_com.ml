(* Ocsigen
 * http://www.ocsigen.org
 * ocsigen_http_com.ml Copyright (C) 2005
 * Denis Berthod, Vincent Balat, JÃ©rÃ´me Vouillon
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

(*
TODO
- server.ml
- shorter timeouts for keep-alive
- check the code against the HTTP spec
- defunctorize sender code
- rewrite HTTP parser
- check HTTP version (at the moment, we always respond with HTTP/1.1!!!)
- for possibly large reads/writes on the network (streaming of more
  than a few kilobytes), we should use Lwt_unix.wait_read and
  Lwt_unix.wait_write: this gives some time for other requests to be
  handled, and the read/write is resumed only when necessary

PERSISTENT CONNECTIONS
======================
http://www.tools.ietf.org/html/draft-ietf-http-connection-00
*)

open Ocsigen_lib

open Ocsigen_http_frame
open Ocsigen_cookies

let section = Lwt_log.Section.make "ocsigen:http:com"

(** this module provide a mecanism to communicate with some http frames *)

let (>>=) = Lwt.(>>=)

(****)

(** Internal exceptions *)
exception Buffer_full

(** Exported exceptions *)
exception Connection_closed
exception Lost_connection of exn
exception Timeout
exception Keepalive_timeout
exception Aborted

(*XXX Provide the max size? *)
let request_too_large max =
  Ocsigen_http_frame.Http_error.Http_exception
    (413, Some "request contents too large", None)

let convert_io_error e =
  match e with
    Unix.Unix_error(Unix.ECONNRESET,_,_)
  | Ssl.Read_error (Ssl.Error_syscall | Ssl.Error_ssl)
  | End_of_file
  | Ssl.Write_error (Ssl.Error_zero_return | Ssl.Error_syscall | Ssl.Error_ssl)
  | Unix.Unix_error (Unix.EPIPE, _, _) ->
    Lost_connection e
  | _ ->
    e

let catch_io_errors f =
  Lwt.catch f (fun e -> Lwt.fail (convert_io_error e))

(****)

type mode = Answer | Query | Nofirstline

type waiter =
  { w_wait : unit Lwt.t;
    w_waker: unit Lwt.u option;
    mutable w_did_wait : bool }

let create_waiter block =
  if block then
    let (t, u) = Lwt.wait () in
    { w_wait = t; w_waker = Some u; w_did_wait = false }
  else
    { w_wait = Lwt.return (); w_waker = None; w_did_wait = false }

(** Communication buffer to receive messages. *)
type connection =
  { id : int;
    fd : Lwt_ssl.socket;
    chan : Lwt_io.output_channel;
    timeout : Lwt_timeout.t;
    r_mode : mode;
    closed : unit Lwt.t * unit Lwt.u;
    mutable buf : string;
    mutable read_pos : int;
    mutable write_pos : int;
    mutable read_mutex : Lwt_mutex.t;
    mutable extension_mutex : Lwt_mutex.t; (* to keep requests in right order *)
    mutable senders : waiter;
    mutable sender_count : int }

let connection_id x = x.id
let connection_fd x = x.fd

let new_id =
  let c = ref 0 in
  fun () -> incr c; !c

let create_receiver timeout mode fd =
  let buffer_size = Ocsigen_config.get_netbuffersize () in
  let timeout =
    Lwt_timeout.create
      timeout
      (fun () -> Lwt_ssl.abort fd Timeout) in
  let buffer, release = Ocsigen_buffer_pool.get_lwt_bytes buffer_size in
  let buf, release2   = Ocsigen_buffer_pool.get_bytes buffer_size in
  { id = new_id ();
    fd = fd;
    chan =
      Lwt_io.make
        ~mode:Lwt_io.output
        ~close:(fun () -> Lwt_timeout.stop timeout;
                          release ();
                          release2 ();
                          Lwt.return_unit)
        ~buffer
        (fun buf pos len ->
           Lwt_timeout.start timeout;
           Lwt.try_bind
             (fun () -> Lwt_ssl.write_bytes fd buf pos len)
             (fun l  -> Lwt_timeout.stop timeout; Lwt.return l)
             (fun e  -> Lwt_timeout.stop timeout;
               Lwt.fail (convert_io_error e)));
    timeout = timeout;
    r_mode = mode;
    buf;
    read_pos = 0;
    write_pos = 0;
    closed = Lwt.wait ();
    read_mutex = Lwt_mutex.create ();
    extension_mutex = Lwt_mutex.create ();
    senders = create_waiter false;
    sender_count = 0 }


(*XXX Do we really need to export this function? *)
let lock_receiver receiver = Lwt_mutex.lock receiver.read_mutex

let unlock_receiver receiver = Lwt_mutex.unlock receiver.read_mutex

let abort conn =
  Lwt.wakeup (snd conn.closed) ();
  Lwt_ssl.abort conn.fd Aborted;
  (* we discard the receive buffer so as to avoid errors in [receive] and such *)
  conn.read_pos <- 0;
  conn.write_pos <- 0;
  (* so that we don't overwrite the buffer we no longer own by accident *)
  conn.buf <- Bytes.create 8;
  ignore (try Lwt_io.close conn.chan with _ -> Lwt.return_unit)

let discard conn =
  (* we discard the receive buffer so as to avoid errors in [receive] and such *)
  conn.read_pos <- 0;
  conn.write_pos <- 0;
  (* so that we don't overwrite the buffer we no longer own by accident *)
  conn.buf <- Bytes.create 8;
  ignore (try Lwt_io.close conn.chan with _ -> Lwt.return_unit)

let closed conn = fst conn.closed

let wakeup_next_request conn = Lwt_mutex.unlock conn.extension_mutex

let block_next_request conn = Lwt_mutex.lock conn.extension_mutex

(****)

(** the number of byte in the buffer*)
let buf_used buffer = buffer.write_pos - buffer.read_pos
let buf_size buffer = Bytes.length buffer.buf

let buf_get_string buffer len =
  let pos = buffer.read_pos in
  assert (pos + len <= buffer.write_pos);
  buffer.read_pos <- buffer.read_pos + len;
  Bytes.sub buffer.buf pos len

(** Receive some more data. *)
let receive receiver =
  let used = buf_used receiver in
  let free = buf_size receiver - used in
  if free = 0 then
    Lwt.fail Buffer_full
  else begin
    if receiver.read_pos > 0 then begin
      Bytes.blit receiver.buf receiver.read_pos receiver.buf 0 used;
      receiver.write_pos <- used;
      receiver.read_pos <- 0
    end;
    if receiver.sender_count = 0 then Lwt_timeout.start receiver.timeout;
    Lwt_ssl.read receiver.fd receiver.buf receiver.write_pos free
    >>= fun len ->
    Lwt_timeout.stop receiver.timeout;
    receiver.write_pos <- used + len;
    if len = 0 then
      Lwt.fail End_of_file
    else begin
      Lwt.return ()
    end
  end

(** Receive data until at least [len] chars are available *)
let rec fill receiver len =
  if buf_used receiver >= len then
    Lwt.return ()
  else begin
    receive receiver >>= fun () ->
    fill receiver len
  end

(****)

type size = Exact of int64 | Bounded of int64 option

let rec extract_aux receiver pos bound cont =
  let avail = buf_used receiver in
  if avail = 0 then
    Lwt.try_bind
      (fun () -> receive receiver)
      (fun () -> extract_aux receiver pos bound cont)
      (fun e ->
         match e, bound with
           End_of_file, Bounded _ ->
           Ocsigen_stream.empty None
         | _ ->
           Lwt.fail (convert_io_error e))
  else
    let pos' = Int64.add pos (Int64.of_int avail) in
    match bound with
      Exact l when pos' >= l ->
      let len = Int64.to_int (Int64.sub l pos) in
      let s = buf_get_string receiver len in
      Ocsigen_stream.cont s cont
    | Bounded (Some l) when pos' > l ->
      Lwt.fail (request_too_large l)
    | _ ->
      let s = buf_get_string receiver avail in
      Ocsigen_stream.cont s (fun () -> extract_aux receiver pos' bound cont)

(** Stream from the receiver channel. *)
let extract receiver bound =
  Ocsigen_stream.make (fun () ->
      extract_aux receiver 0L bound
        (fun () ->
           Lwt_mutex.unlock receiver.read_mutex;
           Ocsigen_stream.empty None))

type pat_res = Found of int | Retry of int

(** Wait for a given pattern to be received *)
let rec wait_pattern find_pattern receiver cur_pos =
  let read_pos = receiver.read_pos in
  let avail = receiver.write_pos - (cur_pos + read_pos) in
  match find_pattern receiver.buf (cur_pos + read_pos) avail with
    Found end_pos ->
    Lwt.return (end_pos - read_pos)
  | Retry retry_pos ->
    let pos = max 0 (retry_pos - read_pos) in
    receive receiver >>= fun () ->
    wait_pattern find_pattern receiver pos

(** Find the first sequence crlfcrlf or lflf in the buffer *)
let rec find_header buf pos rem =
  if rem < 2 then
    Retry (pos - 3)
  else if buf.[pos + 1] <> '\n' then
    find_header buf (pos + 1) (rem - 1)
  else if buf.[pos] = '\n' then
    Found (pos + 2)
  else if
    rem >= 4 && buf.[pos] = '\r' &&
    buf.[pos + 2] = '\r' && buf.[pos + 3] = '\n'
  then
    Found (pos + 4)
  else
    find_header buf (pos + 1) (rem - 1)

(** Wait until a full header is received.  Returns the length of
    the header *)
let wait_http_header receiver =
  Lwt.catch
    (fun () -> wait_pattern find_header receiver 0)
    (fun e ->
       Lwt.fail
         (match e with
            Buffer_full ->
            Ocsigen_http_frame.Http_error.Http_exception
              (413, Some "header too long", None)
          | End_of_file when buf_used receiver = 0 ->
            Connection_closed
          | Timeout when buf_used receiver = 0 && receiver.sender_count = 0 ->
            Keepalive_timeout
          | _ ->
            convert_io_error e))

(** Find an end of line crlf or lf in the buffer *)
let rec find_line buf pos rem =
  if rem < 1 then Retry pos else
  if buf.[pos] = '\n' then Found (pos + 1) else
    find_line buf (pos + 1) (rem - 1)

(** Wait until a full line is received.
    Returns the length of the line *)
let wait_line receiver = wait_pattern find_line receiver 0

(** extract chunked data in destructive way from the buffer.
    The optional [?finish] parameter is an action that
    will be executed when the stream is finished.
*)
let extract_chunked receiver =
  let ec_fail e =
    let e =
      if e = Buffer_full then
        Ocsigen_http_frame.Http_error.Http_exception
          (400, Some "bad chunked data", None)
      else
        convert_io_error e
    in
    Lwt.fail e
  in
  let extract_crlf receiver =
    Lwt.catch
      (fun () ->
         fill receiver 2 >>= fun () ->
         let pos = receiver.read_pos in
         if
           receiver.buf.[pos] = '\r' && receiver.buf.[pos + 1] = '\n'
         then begin
           receiver.read_pos <- pos + 2;
           Lwt.return ()
         end else
           Lwt.fail
             (Ocsigen_http_frame.Http_error.Http_exception
                (400, Some "bad chunked data", None)))
      ec_fail
  in
  let rec aux () =
    Lwt.try_bind (fun () -> wait_line receiver)
      (fun len ->
         let chunksize = buf_get_string receiver len in
         (*XXX Should check that we really have chunked data *)
         let chunksize = Scanf.sscanf chunksize "%x" (fun x -> x) in
         if chunksize = 0 then begin
           extract_crlf receiver >>= fun () ->
           Lwt_mutex.unlock receiver.read_mutex;
           Ocsigen_stream.empty None
         end else
           extract_aux receiver 0L (Exact (Int64.of_int chunksize))
             (fun () -> extract_crlf receiver >>= fun () -> aux ()))
      ec_fail
  in
  Ocsigen_stream.make aux

(* RFC2616, sect 4.3 *)
let code_without_message_body code =
  (code >= 100 && code < 200) || code = 204 || code = 304

let parse_http_header mode s =
  (*XXX Should check that the message corresponds to the mode *)
  let lexbuf = Lexing.from_string s in
  try
    Lwt.return
      (if mode = Nofirstline then
         Http_lexer.nofirstline
           { Ocsigen_http_frame.Http_header.mode =
               Ocsigen_http_frame.Http_header.Nofirstline;
             proto =
               Ocsigen_http_frame.Http_header.HTTP11;
             headers = Http_headers.empty }
           lexbuf
       else
         Http_lexer.header lexbuf)
  with Parsing.Parse_error ->
    Lwt.fail (Ocsigen_http_frame.Http_error.Http_exception
                (400, Some "parse error", None))

let get_maxsize = function
  | Nofirstline
  | Answer -> None (* Ocsigen_config.get_maxanswerbodysize ()
                      Do we need a limit?
                      If yes, add an exception Ocsigen_Answer_too_long.
                      (like Ocsigen_Request_too_long)
                   *)
  | Query -> Ocsigen_config.get_maxrequestbodysize ()


let return_with_no_body receiver = Lwt.return None


(** get an http frame *)
let get_http_frame ?(head = false) receiver =

  Lwt_mutex.lock receiver.read_mutex >>= fun () ->
  wait_http_header receiver >>= fun len ->
  let string_header = buf_get_string receiver len in
  parse_http_header receiver.r_mode string_header >>= fun header ->
  (* RFC2616, sect 4.4
     1.  Any response message  which "MUST  NOT" include  a message-body
     (such as the 1xx, 204, and 304 responses and any response to a HEAD
     request) is  always terminated  by the first  empty line  after the
     header fields,  regardless of  the entity-header fields  present in
     the message.
  *)
  begin match header.Ocsigen_http_frame.Http_header.mode with
    | Ocsigen_http_frame.Http_header.Answer code
      when code_without_message_body code ->
      return_with_no_body receiver
    | _ ->
      if head then begin
        return_with_no_body receiver
      end
      else begin
        (* RFC
           2. If  a Transfer-Encoding header field (section  14.41) is present
           and has  any value other than "identity",  then the transfer-length
           is defined  by use of the "chunked"  transfer-coding (section 3.6),
           unless the message is terminated by closing the connection.
        *)
        let chunked =
          try
            Ocsigen_http_frame.Http_header.get_headers_value
              header Http_headers.transfer_encoding <> "identity"
          with Not_found ->
            false
        in
        if chunked then
          Lwt.return (Some (extract_chunked receiver))
        else begin
          (* RFC
             3. If a Content-Length header field (section 14.13) is present, its
             decimal value  in OCTETs represents both the  entity-length and the
             transfer-length. The  Content-Length header field MUST  NOT be sent
             if these  two lengths are  different (i.e., if  a Transfer-Encoding
             header  field is present).  If a  message is  received with  both a
             Transfer-Encoding header  field and a  Content-Length header field,
             the latter MUST be ignored.
          *)
          let content_length =
            try
              (*XXX Check for overflow/malformed field... *)
              Some
                (Int64.of_string
                   (Ocsigen_http_frame.Http_header.get_headers_value
                      header Http_headers.content_length))
            with Not_found ->
              None
          in
          match content_length with
          | Some cl ->
            if cl < 0L then
              (*XXX Malformed field!!!*)
              Lwt.fail
                (Ocsigen_http_frame.Http_error.Http_exception
                   (400, Some "ill-formed content-length header", None))
            else if cl = 0L then
              return_with_no_body receiver
            else
              let max = get_maxsize receiver.r_mode in
              begin match max with
                  Some m when cl > m ->
                  Lwt.fail (request_too_large m)
                | _ ->
                  Lwt.return (Some (extract receiver (Exact cl)))
              end
          | None ->
            (* RFC
               4. If  the message uses the media  type "multipart/byteranges", and
               the transfer-length is  not  otherwise specified,  then this  self-
               delimiting media type defines the transfer-length.  This media type
               MUST NOT be used unless the sender knows that the recipient can parse
               it; the presence in a request  of a Range header with multiple byte-
               range specifiers from a 1.1 client implies that the client can parse
               multipart/byteranges responses.
               NOT IMPLEMENTED

               5. By  the server closing  the connection. (Closing  the connection
               cannot be  used to indicate the  end of a request  body, since that
               would leave no possibility for the server to send back a response.)
            *)
            match header.Ocsigen_http_frame.Http_header.mode with
              Ocsigen_http_frame.Http_header.Query (_, s) ->
              return_with_no_body receiver
            | _ ->
              let st =
                extract receiver
                  (Bounded (get_maxsize receiver.r_mode)) in
              Lwt.return (Some st)
        end
      end
  end >>= fun b ->
  let la =
    (match b with
     | None -> Lwt_mutex.unlock receiver.read_mutex; None
     | Some s ->
       Ocsigen_stream.add_finalizer s (fun _ -> Ocsigen_stream.consume s);
       Some s
    )
  in
  Lwt.return {Ocsigen_http_frame.frame_header = header;
              frame_content = la;
              frame_abort =
                (fun () -> Lwt_ssl.close receiver.fd)}
(*VVV close or shutdown? *)

(****)

type slot =
  { sl_waiter : waiter;
    sl_chan : Lwt_io.output_channel;
    sl_ssl : bool (* for secure cookies only *)}

let create_slot conn =
  { sl_waiter = conn.senders;
    sl_chan   = conn.chan;
    sl_ssl = Lwt_ssl.is_ssl conn.fd}

(****)

let start_processing conn f =
  let slot = create_slot conn in
  let next_waiter = create_waiter true in
  conn.senders <- next_waiter;
  conn.sender_count <- conn.sender_count + 1;
  Lwt_timeout.stop conn.timeout;
  ignore (* We can ignore the thread as all the exceptions are caught *)
    (Lwt.try_bind
       (fun () ->
          Lwt.finalize
            (fun () ->
               (* If we want to serialize query processing, we can call
                  [wait_previous_senders slot] here.  But then, we should
                  also flush the channel sooner. *)
               f slot >>= (fun () ->
                   (*XXX Check that we waited: slot.sl_did_wait = true *)
                   (*XXX It would be clearer to put this code at the end of the sender function,
                         but we don't have access to [next_slot] there *)
                   if not next_waiter.w_did_wait then
                     Lwt_io.flush conn.chan
                   else
                     Lwt.return ()))
            (fun () ->
               conn.sender_count <- conn.sender_count - 1;
               if conn.sender_count = 0 then Lwt_timeout.start conn.timeout;
               Lwt.return ()))
       (fun () ->
          (match next_waiter.w_waker with
           | None -> ()
           | Some wk -> Lwt.wakeup wk ())
        ; Lwt.return ()
       )
       (fun e ->
          (match next_waiter.w_waker with
           | None -> ()
           | Some wk -> Lwt.wakeup_exn wk e)
        ; Lwt.return ()
       ))

let wait_previous_senders slot =
  slot.sl_waiter.w_did_wait <- true;
  slot.sl_waiter.w_wait

let wait_all_senders conn =
  Lwt.finalize
    (fun () ->
       Lwt.catch
         (*XXX Do we need a flush here?  Are we properly flushing in case of an error? *)
         (fun () ->
            conn.senders.w_wait >>= fun () ->
            Lwt_io.flush conn.chan)
         (fun e -> match e with Aborted -> Lwt.return () | _ -> Lwt.fail e))
    (fun () ->
       Lwt_timeout.stop conn.timeout;
       Lwt.return ())



let (<<) h (n, v) = Http_headers.replace n v h

let (<<?!) h (n, v) = Http_headers.replace_opt n v h
(* None means: remove from headers *)

let (<<?) h (n, v) =
  match v with
  | None -> h (* None means: do not change the value *)
  | Some v -> Http_headers.replace n v h

let gmtdate d =
  let x = Netdate.mk_mail_date ~zone:0 d in try
    (*XXX !!!*)
    let ind_plus =  Bytes.index x '+' in
    Bytes.set x ind_plus 'G';
    Bytes.set x (ind_plus + 1) 'M';
    Bytes.set x (ind_plus + 2) 'T';
    String.sub x 0 (ind_plus + 3)
  with Invalid_argument _ | Not_found -> Lwt_log.ign_debug ~section "no +"; x

type sender_type = {
  (** protocol to be used : HTTP/1.0 HTTP/1.1 *)
  mutable s_proto: Ocsigen_http_frame.Http_header.proto;
  (** the options to send with each frame, for exemple : server name , ... *)
  mutable s_headers: Http_headers.t
}

(** create a new sender *)
let create_sender
    ?server_name
    ?(headers=Http_headers.empty)
    ?(proto=Ocsigen_http_frame.Http_header.HTTP11)
    () =
  let headers =
    Http_headers.replace Http_headers.accept_ranges "none" headers in
  let headers =
    Http_headers.replace_opt Http_headers.server server_name headers in
  { s_headers = headers; s_proto = proto }


let default_sender = create_sender ~server_name:Ocsigen_config.server_name ()

(* Old version
   (* XXX Maybe we should merge small strings *)
   (* XXX We should probably make sure that any exception raised by
   the stream is properly caught *)
   let rec write_stream_chunked out_ch stream =
   Ocsigen_stream.next stream >>= fun e ->
   match e with
    Ocsigen_stream.Finished _ ->
      Lwt_io.write out_ch "0\r\n\r\n"
   | Ocsigen_stream.Cont (s, next) ->
      let l = String.length s in
      begin if l = 0 then
        (* It is incorrect to send an empty chunk *)
        Lwt.return ()
      else begin
        Lwt_io.write out_ch (Format.sprintf "%x\r\n" l) >>= fun () ->
        Lwt_io.write out_ch s >>= fun () ->
        Lwt_io.write out_ch "\r\n"
      end end >>= fun () ->
      write_stream_chunked out_ch next
*)

(* XXX We should probably make sure that any exception raised by
   the stream is properly caught *)
(* 20071128 Current xhtml pretty printer is making lots of very small strings.
   We bufferise them before creating a thunk.
   Benchmarks cannot prove that it is better, but at least the network stream
   is readable ...
   It is then buffered again by Lwt_io.
   Is there a way to have only one buffer?
*)
let write_stream_chunked out_ch stream =
  let buf_size = 4096 in
  let size_for_not_buffering = 900 in
  let buffer, release = Ocsigen_buffer_pool.get_bytes buf_size in
  let rec aux stream len =
    Ocsigen_stream.next stream >>= fun e ->
    match e with
    | Ocsigen_stream.Finished _ ->
      (if len > 0 then begin
          (* It is incorrect to send an empty chunk *)
          Lwt_io.write
            out_ch (Format.sprintf "%x\r\n" len) >>= fun () ->
          Lwt_io.write_from_exactly out_ch buffer 0 len >>= fun () ->
          Lwt_io.write out_ch "\r\n"
        end else
         Lwt.return ()) >>= fun () ->
      Lwt_io.write out_ch "0\r\n\r\n" >>= fun () ->
      release ();
      Lwt.return_unit
    | Ocsigen_stream.Cont (s, next) ->
      let l = String.length s in
      if l = 0 then
        aux next len
      else
      if l >= size_for_not_buffering then begin
        (if len > 0 then begin
            Lwt_io.write
              out_ch (Format.sprintf "%x\r\n" len) >>= fun () ->
            Lwt_io.write_from_exactly out_ch buffer 0 len >>= fun () ->
            Lwt_io.write out_ch "\r\n"
          end else Lwt.return ()) >>= fun () ->
        Lwt_io.write
          out_ch (Format.sprintf "%x\r\n" l) >>= fun () ->
        Lwt_io.write_from_exactly out_ch s 0 l >>= fun () ->
        Lwt_io.write out_ch "\r\n" >>= fun () ->
        aux next 0
      end else (* Will not work if l is very large: *)
        let available = buf_size - len in
        if l > available then begin
          Lwt_io.write
            out_ch (Format.sprintf "%x\r\n" buf_size) >>= fun () ->
          Lwt_io.write_from_exactly out_ch buffer 0 len >>= fun () ->
          Lwt_io.write_from_exactly out_ch s 0 available >>= fun () ->
          Lwt_io.write out_ch "\r\n" >>= fun () ->
          let newlen = l - available in
          String.blit s available buffer 0 newlen;
          aux next newlen
        end
        else begin
          String.blit s 0 buffer len l;
          aux next (len + l)
        end
  in
  aux stream 0

let rec write_stream_raw out_ch stream =
  Ocsigen_stream.next stream >>= fun e ->
  match e with
  | Ocsigen_stream.Finished _ ->
    Lwt.return ()
  | Ocsigen_stream.Cont (s, next) ->
    Lwt_io.write out_ch s >>= fun () ->
    write_stream_raw out_ch next

(*XXX We should check the length of the stream:
  - do not send more than expected
  - abort the connection before the right length is emitted so that
    the client can know something wrong happened
*)
let write_stream ?(chunked=false) out_ch stream =
  let stream = Ocsigen_stream.get stream in
  if chunked then
    write_stream_chunked out_ch stream
  else
    write_stream_raw out_ch stream




module H = Ocsigen_http_frame.Http_header

let set_result_observer, observe_result =
  let observer = ref (fun _ _ -> Lwt.return ()) in
  ((fun f -> 
      let o = !observer in
      observer := (fun a b -> o a b >>= fun () -> f a b)),
   (fun a b -> !observer a b))

let send_100_continue slot =
  wait_previous_senders slot >>= fun () ->
  let out_ch = slot.sl_chan in
  let hh = Framepp.string_of_header {
      H.mode = H.Answer 100;
      proto = H.HTTP11;
      headers = Http_headers.empty
    } in
  Lwt_log.ign_info ~section "writing 100-continue";
  Lwt_log.ign_info ~section hh;
  Lwt_io.write out_ch hh

(** Sends the HTTP frame.
 * The headers are merged with those of the sender, the priority
 * being given to the newly defined header in case of conflict.
 * code is the code of the http answer
 * keep_alive is a boolean value that set the field Connection
*)
let send
    ?reopen
    slot
    ~clientproto
    ?mode
    ?proto
    ?keep_alive (* now (06/02/2008) used only
                   to request keep-alive while used as client *)
    ~head (* send only the header *)
    ~sender
    res
  =

  let send_aux ~mode hds =
    Lwt.catch
      (fun () ->
         (* [slot] is here for pipelining: we must wait before
            sending the page, because the previous one may not be sent. *)
         wait_previous_senders slot >>= fun () ->
         let out_ch = slot.sl_chan in
         let empty_content =
           match mode with
           | H.Nofirstline -> false
           | H.Answer code -> code_without_message_body code
           | H.Query _     -> false
         in
         let chunked =
           (Result.content_length res) = None &&
           clientproto <> Ocsigen_http_frame.Http_header.HTTP10 &&
           not empty_content && not head
         in
         (* if HTTP/1.0 we do not use chunked encoding
            even if the client tells that it supports it,
            because it may be an HTTP/1.0 proxy that
            transmits the header by mistake.
            In that case, we close the connection after the
            answer.
         *)
         let with_default v default =
           match v with None -> default | Some v -> v
         in
         (*XXX Make sure that there is no way to put wrong headers *)
         (*VVV and that all required headers are here ... *)
         let hds =
           Http_headers.with_defaults hds sender.s_headers
         in
         let hds =
           Http_headers.replace_opt Http_headers.transfer_encoding
             (if chunked then Some "chunked" else None) hds
         in
         let hds =
           Http_headers.replace_opt Http_headers.content_length
             (match Result.content_length res with
              | None   -> None
              | Some l -> Some (Int64.to_string l))
             hds
         in
         (* 06/02/2008
            We decided that we won't include a connection header at all
            for answers.
            If HTTP/1.0, we always close (which is the default).
            If HTTP/1.1, we always keep alive, but if the client asked to close.
            The default is to keep alive. But the client will close. *)
         let hds =
           match keep_alive with
           | None -> hds
           | Some ka ->
             hds
             <<?! (* We override the value *)
             (*XXX Check: HTTP/1.0 *)
             (Http_headers.connection,
              if ka
              (* && not (not chunked && res.res_content_length = None) *)
              then Some "keep-alive"
              else Some "close")
         in
         (*VVV Do we need to REMOVE the connection header if present? *)
         let hd =
           { H.mode = mode;
             H.proto = with_default proto sender.s_proto;
             H.headers = hds }
         in


         catch_io_errors
           (fun () ->
              Lwt.catch
                (fun () ->

                  let hh = Framepp.string_of_header hd in
                  Lwt_log.ign_info_f ~section "writing header\n%s" hh;
                  observe_result hd hh >>= fun () ->
                  Lwt_io.write out_ch hh >>= fun () ->
                   (if reopen <> None then
                      (* If we want to give a possibility to reopen if
                         it fails, we must detect the failure before
                         beginning to read the stream
                      *)
                      Lwt_io.flush out_ch
                    else Lwt.return ())
                )
                (fun e -> (* *** If we are doing a request,
                             we may want to retry once (in the case when
                             we reuse an old connection) *** *)
                   match reopen with
                     | None -> Lwt.fail e
                     | Some reopen ->
                         match convert_io_error e with
                           | Keepalive_timeout
                           | Timeout
                           | Connection_closed
                           | Unix.Unix_error (Unix.EBADF,_ ,_)
                           | Lost_connection _ ->
                               reopen () >>= fun () ->
                               Lwt.fail e
                           | _ ->
                             Lwt_log.ign_warning ~section ~exn:e
                               "reopening after exception (Is that right?) Please report this error.";
                             ignore (reopen ());
                             Lwt.fail e
                )
           )
         >>= fun () ->
         (if empty_content || head then begin
           Lwt.return ()
         end else begin
            Lwt_log.ign_info ~section "writing body";
           write_stream ~chunked out_ch (fst (Result.stream res))
         end) >>= fun () ->
         Lwt_io.flush out_ch (* Vincent: I add this otherwise HEAD answers
                                are not flushed by the reverse proxy *)
         >>= fun () ->
         Ocsigen_stream.finalize (fst (Result.stream res)) `Success
      )
      (fun e ->
         Ocsigen_stream.finalize (fst (Result.stream res)) `Failure >>= fun () ->
         Lwt.fail e
      )

  in

  (*XXX Maybe we can compute this only at most once a second*)
  (* Add options specific to the page. *)
  let date = gmtdate (Unix.time ()) in

  let headers =
    (Result.headers res)
    <<?
    (* We must fetch the last modification. *)
    (Http_headers.last_modified,
     match Result.lastmodified res with
       None    -> None (* We do not put last modified for dynamically
                          generated pages, otherwise it is not possible
                          to cache them.  Without Last-Modified, ETag is
                          taken into account by proxies/browsers *)
     | Some l  -> Some (gmtdate l))
  in
  let mode =
    match mode with
    | None -> Http_header.Answer (Result.code res)
    | Some m -> m
  in
  let headers =
    match mode with
    | H.Query _ -> headers
    (* We do not put date in headers for queries.
       cf bug #134 in trac
       "the header "Date" is sometime used to compute the signature of the REST
       request, and it's value may change between the time you compute the
       signature and the time the request is actually performed."
       RFC 2616 says:
       "Clients SHOULD only send a Date header field in messages that include
       an entity-body, as in the case of the PUT and POST requests, and even
       then it is optional. A client without a clock MUST NOT send a Date
       header field in a request." *)
    (*VVV What about Nofirstline? *)
    | _ -> headers
           <<
           (Http_headers.date, date)
  in
  let mkcook path exp name c secure =
    Format.sprintf "%s=%s%s%s" name c
      (*VVV encode = true? *)
      ("; path=/" ^ Url.string_of_url_path ~encode:true path)
      (if secure && slot.sl_ssl then "; secure" else "")^
    (match exp with
     | Some s -> "; expires=" ^
                 Netdate.format
                   "%a, %d-%b-%Y %H:%M:%S GMT"
                   (Netdate.create s)
     | None   -> "")
  in
  let mkcookl path t hds =
    CookiesTable.fold
      (fun name c h ->
        let exp, v, secure = match c with
        | Ocsigen_cookies.OUnset -> (Some 0., "", false)
        | Ocsigen_cookies.OSet (t, v, secure) -> (t, v, secure)
        in
        Http_headers.add
          Http_headers.set_cookie (mkcook path exp name v secure) h)
      t
      hds
  in
  let headers =
    Cookies.fold mkcookl (Result.cookies res) headers
    <<?
    (Http_headers.location, Result.location res)
    <<?
    (Http_headers.etag,
     match Result.etag res with
     | None   ->  None
     | Some l ->  Some (Format.sprintf "\"%s\"" l))
    (*XXX Is it the right place to perform quoting?*)
    <<?
    (Http_headers.content_type,
     match Result.content_type res with
     | None   -> None
     | Some s ->
       if String.length s >= 4 then
         match String.sub s 0 4, Result.charset res with
         | "text", Some "" -> Some s
         | "text", Some c -> Some (Format.sprintf "%s; charset=%s" s c)
         | _              ->
           match String.sub s (String.length s - 4) 4,
                 Result.charset res with
           | ("+xml"|"/xml"), Some "" -> Some s
           | ("+xml"|"/xml"), Some c ->
             Some (Format.sprintf "%s; charset=%s" s c)
           | _ -> Result.content_type res
       else
         (Result.content_type res)
    )
  in
  send_aux ~mode headers
