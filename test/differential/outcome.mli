(** What an echo server did with one input: the requests it understood, or
    its refusal. *)

type answer =
  | Echoed of Echo.request  (** A 200 response carrying a request. *)
  | Rejected of int  (** A response with another status code. *)
  | Unreadable of string
      (** A 200 response whose body is not an echoed request, with the reason.
      *)

type t =
  { answers : answer list  (** The final responses, in order (1xx skipped). *)
  ; rest : string  (** The bytes received after them that are not one. *)
  ; ending : Raw_http.ending
  }

val of_outcome : Raw_http.outcome -> t
(** [of_outcome o] reads the responses of [o]. A response without length is
    read to the end of [o] when the server closed the connection. *)

type quirks =
  { underscore_names : bool
        (** The server writes the hyphens of header names as underscores, as
          WSGI and CGI do. *)
  }
(** The known ways a server writes what it understood that are not
    differences of understanding. *)

type view =
  | Accepted of Echo.request
      (** Normalized: header names in lowercase, without Content-Length and
          Transfer-Encoding, whose role is played by the body, which is
          compared directly; the fields of the same name joined in one, in
          their order, sorted by name, without whitespace around the commas
          of values. An absolute-form target is replaced by
          its path, and its authority by the Host field, as the server must
          use it (RFC 9112 section 3.2.2). *)
  | Refused  (** Rejected, or not answered. *)
  | Broken
      (** An unreadable echo, or bytes that are not a response. The reason
          is left out: it depends on the server, not on its understanding
          of the input. *)

val view : ?ignored:string list -> ?quirks:quirks -> t -> view list
(** [view t] is what [t] means for the comparison of servers, normalized
    according to [quirks] (default: none) and without the fields named in
    [ignored] (default: none), which some servers add or remove themselves: one view per
    answer, followed by [Broken] if [rest] is not empty, without the [Refused]
    views at the end, so that a server that answers a request with 400 and one
    that closes the connection agree. The empty list means that the input was
    refused as a whole. The ending is left out too: a server that waits for
    more bytes after its last answer, taking the rest of the input for an
    unfinished body, agrees with one that refuses that rest. *)

val clusters :
     ?ignored:string list
  -> (string * quirks * t) list
  -> (string list * view list) list
(** [clusters outcomes] groups the names of [outcomes] whose views, with
    [ignored] as for {!view}, are equal,
    in the order of their first appearance, each group with its views. *)

val view_to_string : view list -> string
(** [view_to_string v] is a printable rendering of [v], ["refused"] for the
    empty list. *)

val to_string : t -> string
(** [to_string t] is a printable rendering of [t], one line per answer, then
    the rest and the ending. *)
