(** Rules of HTTP/1.1 messages (RFC 9112) that cohttp does not enforce. *)

val check_host :
   Http.Version.t
  -> Http.Header.t
  -> target:string
  -> (unit, string) result
(** [check_host version headers ~target] is [Ok ()] iff a request with these
    version, header fields and request target names its host as RFC 9112
    (section 3.2) asks: exactly one [Host] field, or none in HTTP/1.0, whose
    value is a host name, an IPv4 address or an IPv6 address in brackets,
    optionally followed by a port, without percent-encoding; and a target in
    origin form ([/...]), [*], or in absolute form with an [http] or [https]
    URI whose host is valid in the same way. Otherwise it is [Error reason],
    for the logs, and the server must answer 400. *)

val valid_field_values : Http.Header.t -> bool
(** [valid_field_values headers] is [true] iff no value of [headers] has a
    control character other than a horizontal tab (RFC 9110, section 5.5).
    NUL, CR and LF in particular must be rejected. *)

val request_method : Http.Method.t -> Http.Method.t option
(** [request_method meth] is the method of a request that cohttp read with
    the method [meth], or [None] if it is not a valid token. Cohttp does not
    skip the empty lines that may come before a request line (RFC 9112,
    section 2.2), sent for instance by clients that end a POST body with an
    extra line end: it takes them as the start of the method. They are
    removed here. *)
