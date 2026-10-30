(** Raw HTTP exchanges for black-box tests of a server.

    Requests are sent byte for byte, without any normalisation, so that
    malformed or unusual requests reach the server exactly as written. Every
    byte the server sends back is kept, together with the way the exchange
    ended. *)

(** {1 Targets} *)

type tls_version = Tls_1_2 | Tls_1_3

type transport =
  | Plain
  | Tls of {max_version : tls_version; sni : string option}
  (** TLS without certificate verification: the test servers use
          self-signed certificates. *)

type target = {host : string; port : int; transport : transport}

(** {1 Connections} *)

type conn
(** An open connection to a target. *)

exception Connect_failed of string
(** Raised by {!connect} when the TCP connection or the TLS handshake fails.
    The argument is a short, stable description of the failure. *)

val connect : target -> conn
(** [connect target] opens a connection to [target], and performs the TLS
    handshake when [target.transport] is [Tls _]. *)

val send : ?trickle:int * float -> conn -> string -> bool
(** [send conn data] writes [data] on [conn]. With [~trickle:(n, d)], [data]
    is sent in pieces of [n] bytes with a pause of [d] seconds between two
    pieces. The result is [false] when the server closed or reset the
    connection before all of [data] was written. *)

type ending =
  | Closed  (** The server closed the connection. *)
  | Reset  (** The connection was reset by the server. *)
  | Idle  (** Nothing arrived during the idle delay; the connection is open. *)
  | Overflow  (** The server sent more than the reception limit. *)

type outcome = {received : string; ending : ending}

type framing =
  | Complete of {close : bool}
  (** One or more complete responses, the last one final (not 1xx), and
          nothing else. [close] tells whether the last one closes the
          connection ([Connection: close], or HTTP/1.0 without keep-alive). *)
  | Incomplete
  (** The last response, or the final one that must follow a 1xx, is not
          complete yet; or nothing has been received. *)
  | Unframed
  (** Not responses, or a response whose end is the end of the
          connection, or that switches to another protocol (101). *)

val framing : string -> framing
(** [framing received] tells whether [received] ends at the end of a final
    response, as delimited by Content-Length or the chunked coding (RFC 9112
    section 6.3); 1xx, 204 and 304 responses have no content. The request
    method is not known, so the response to a HEAD request is complete only
    when as many bytes as its Content-Length follow it. *)

val receive : ?idle:float -> ?settle:float -> ?limit:int -> conn -> outcome
(** [receive conn] reads from [conn] until the server closes the connection,
    until nothing arrives for [idle] seconds (default [2.]), or until [limit]
    bytes have been received (default 1 MiB).

    When the bytes received so far are complete responses (see {!framing})
    and the last one leaves the connection open, the delay is [settle]
    seconds, at most [idle] (default [idle]). Bytes that follow within that
    delay, such as content sent after a 204 or 304 response, or a response to
    a pipelined request, are still seen; later ones are not. After a response
    that closes the connection ([Connection: close], or HTTP/1.0 without
    keep-alive), the close is waited for [idle] seconds. *)

val close : conn -> unit
(** [close conn] closes [conn]. It never fails. *)

val exchange :
   ?trickle:int * float
  -> ?idle:float
  -> ?settle:float
  -> ?limit:int
  -> target
  -> string
  -> outcome
(** [exchange target request] connects to [target], sends [request], receives
    the answer as {!receive} does, and closes the connection. When the server
    stops the connection before the whole request is sent, the bytes it sent
    are still returned. *)

(** {1 Rendering} *)

val render : ?status_only:bool -> ?validators:bool -> outcome -> string
(** [render outcome] is a printable rendering of [outcome] that does not
    depend on the time of the test:
    - line ends ["\r\n"] become ["\n"], other control and non-ASCII bytes
      are written [\xHH];
    - header lines [Date], [ETag] and [Last-Modified] are removed, wherever
      they appear;
    - the last line describes {!ending}: [[closed]], [[reset]], [[idle]] or
      [[overflow]].

    With [~status_only:true], only the status lines ([HTTP/...]) are kept,
    followed by the ending line. With [~validators:true], the [ETag] and
    [Last-Modified] lines are kept, for tests that fix the modification time
    of the files they request. *)

val status_codes : string -> int list
(** [status_codes received] is the list of status codes of the responses in
    [received], in order: the three digits that follow the version in status
    lines. Only lines that start with ["HTTP/"] at the beginning of the data or
    after a line end are considered, so this is a heuristic when bodies
    contain such lines. *)
