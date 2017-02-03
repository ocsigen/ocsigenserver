(** Using Ocsigen as a HTTP client *)

(** Do a GET HTTP request.

    The default port is 80 for HTTP, 443 for HTTPS.

    The default protocol is http ([https=false]). *)
val get :
    ?https: bool ->
    ?port:int ->
    ?headers: Cohttp.Header.t ->
    host:string ->
    uri:string ->
    unit ->
    (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

(** Do a POST HTTP request.
    The default port is 80 for HTTP, 443 for HTTPS.
    The default protocol is http ([https=false]).
    Warning: the stream must be finalized manually after reading, using
    {!Ocsigen_stream.finalize}, otherwise you will have fd leaks.
*)
val post_string :
  ?https: bool ->
  ?port:int ->
  ?headers: Cohttp.Header.t ->
  host:string ->
  uri:string ->
  content:string ->
  content_type:(string * string) ->
  unit ->
  (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

(** Do a POST HTTP request with URL encoded parameters as content.
    The default port is 80 for HTTP, 443 for HTTPS.
    The default protocol is http ([https=false]).
    Warning: the stream must be finalized manually after reading, using
    {!Ocsigen_stream.finalize}, otherwise you will have fd leaks.
*)
val post_urlencoded :
  ?https: bool ->
  ?port:int ->
  ?headers: Cohttp.Header.t ->
  host:string ->
  uri:string ->
  content:(string * string) list ->
  unit ->
  (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

val raw_request :
  ?keep_alive: bool ->
  ?headers: Cohttp.Header.t ->
  ?https: bool ->
  ?port:int ->
  content: string Ocsigen_stream.t option ->
  ?content_length: int64 ->
  meth:Cohttp.Code.meth ->
  host:string ->
  inet_addr:Unix.inet_addr ->
  uri:string ->
  unit ->
  unit ->
  (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
(**
   Do an HTTP request (low level).

   If the optional argument [headers] is present, no headers will be
   added by Ocsigen, but those in this argument and host, and
   [connection: close] or [connection: keep-alive].
   Be carefull to respect HTTP/1.1 in this case!
   ([host] is the full Host HTTP field to send).

   The default port is 80 for HTTP, 443 for HTTPS.

   The default protocol is http ([https=false]).

   The parameters [?keep_alive] and [~inet_addr] are ignored.
*)

val basic_raw_request :
  ?headers: Cohttp.Header.t ->
  ?https: bool ->
  ?port:int ->
  content: string Ocsigen_stream.t option ->
  ?content_length: int64 ->
  meth:Cohttp.Code.meth ->
  host:string ->
  inet_addr:Unix.inet_addr ->
  uri:string ->
  unit ->
  (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
(** Same as {!Ocsigen_http_client.raw_request},
    but does not try to reuse connections.
    Opens a new connections for each request. Far less efficient.
*)
