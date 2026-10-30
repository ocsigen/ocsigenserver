# Module `Ocsigen.Multipart`

```ocaml
val section : Logs.src
```
```ocaml
val scan_multipart_body_from_stream : 
  ?max_size:Stdlib.Int64.t ->
  boundary:string ->
  create:((string * string) list -> 'a) ->
  add:('a -> string -> unit Lwt.t) ->
  stop:(int64 -> 'a -> 'b Lwt.t) ->
  string Ocsigen_base.Ocsigen_stream.stream ->
  unit Lwt.t
```
```ocaml
type content_type = (string * string) * (string * string) list
```
```ocaml
type file_info = {
  tmp_filename : string;
  filesize : int64;
  raw_original_filename : string;
  file_content_type : content_type option;
}
```
```ocaml
type post_data = (string * string) list * (string * file_info) list
```
```ocaml
val post_params : 
  content_type:content_type ->
  on_file:(string -> unit) ->
  Cohttp_lwt.Body.t ->
  (string option -> Stdlib.Int64.t option -> post_data Lwt.t) option
```
`post_params ~content_type ~on_file body` is the function that decodes `body`, if `content_type` is a form encoding. The returned function takes the directory where to store the uploaded files (`None` forbids uploads) and their maximum size. It calls `on_file` with the name of each file it creates, which the caller removes once it no longer needs it.

```ocaml
val parse_content_type : string -> content_type option
```
`parse_content_type s` is the media type and the parameters of the Content-Type header value `s`, or `None` if it has no media type. The names of the parameters are in lowercase, and their values without quotes or escapes. Malformed parameters are ignored.
