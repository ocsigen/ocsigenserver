val get :
    ?v6:bool ->
    ?https: bool ->
    ?port:int ->
    ?headers: Http_headers.t ->
    host:string ->
    uri:string ->
    unit ->
    Ocsigen_http_frame.t Lwt.t

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
