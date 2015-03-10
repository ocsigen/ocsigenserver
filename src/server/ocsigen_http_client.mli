(** Using Ocsigen as a HTTP client *)

(** Do a GET HTTP request.
    The default port is 80 for HTTP, 443 for HTTPS.
    The default protocol is http ([https=false]).
    Warning: the stream must be finalized manually after reading, using
    {!Ocsigen_stream.finalize}, otherwise you will have fd leaks.
*)
val get :
    ?v6:bool ->
    ?https: bool ->
    ?port:int ->
    ?headers: Http_headers.t ->
    host:string ->
    uri:string ->
    unit ->
    Ocsigen_http_frame.t Lwt.t

(** Do a POST HTTP request.
    The default port is 80 for HTTP, 443 for HTTPS.
    The default protocol is http ([https=false]).
    Warning: the stream must be finalized manually after reading, using
    {!Ocsigen_stream.finalize}, otherwise you will have fd leaks.
*)
val post_string :
  ?v6:bool ->
  ?https: bool ->
  ?port:int ->
  ?headers: Http_headers.t ->
  host:string ->
  uri:string ->
  content:string ->
  content_type:(string * string) ->
  unit ->
  Ocsigen_http_frame.t Lwt.t

(** Do a POST HTTP request with URL encoded parameters as content.
    The default port is 80 for HTTP, 443 for HTTPS.
    The default protocol is http ([https=false]).
    Warning: the stream must be finalized manually after reading, using
    {!Ocsigen_stream.finalize}, otherwise you will have fd leaks.
*)
val post_urlencoded :
  ?v6:bool ->
  ?https: bool ->
  ?port:int ->
  ?headers: Http_headers.t ->
  host:string ->
  uri:string ->
  content:(string * string) list ->
  unit ->
  Ocsigen_http_frame.t Lwt.t
