val scan_multipart_body_from_stream:
  ?max_size : Int64.t ->
  boundary : string ->
  create : ((string * string) list -> 'a) ->
  add : ('a -> string -> unit Lwt.t) ->
  stop : (int64 -> 'a -> 'b Lwt.t) ->
  string Ocsigen_stream.stream ->
  unit Lwt.t

type content_type = (string * string) * (string * string) list

type file_info = {
  tmp_filename : string ;
  filesize : int64 ;
  raw_original_filename : string ;
  file_content_type : content_type option
}

type post_data = (string * string) list * (string * file_info) list

val post_params :
  content_type : content_type ->
  Cohttp_lwt.Body.t ->
  (string option -> Int64.t option -> post_data Lwt.t) option

val parse_content_type : string -> content_type option
