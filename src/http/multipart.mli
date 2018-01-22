
val scan_multipart_body_from_stream:
  bytes Ocsigen_stream.stream ->
  boundary:string ->
  create:((string * string) list -> 'a) ->
  add:('a -> bytes -> unit Lwt.t) ->
  stop:(int64 -> 'a -> 'b Lwt.t) ->
  maxsize:Int64.t option -> unit Lwt.t
