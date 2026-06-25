
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
  Cohttp_lwt.Body.t ->
  (string option -> Stdlib.Int64.t option -> post_data Lwt.t) option
```
```ocaml
val parse_content_type : string -> content_type option
```