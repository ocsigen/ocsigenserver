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
