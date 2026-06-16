
# Module `Ocsigen_request`

```ocaml
type t
```
```ocaml
type content_type = (string * string) * (string * string) list
```
```ocaml
type file_info = Ocsigen_multipart.file_info = {
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
type client_conn = [ 
  | `Inet of Ipaddr.t * int
  | `Unix of string
  | `Forwarded_for of string
  | `Unknown
 ]
```
Type of connection used by the client. ``Inet` means the client connected through the Internet. ``Forwarded_for` means that the client connected through a proxy and carries the IP address reported in the HTTP headers.

```ocaml
val make : 
  ?forward_ip:string list ->
  ?sub_path:string ->
  ?original_full_path:string ->
  ?request_cache:Polytables.t ->
  ?cookies_override:string Ocsigen_cookie_map.Map_inner.t ->
  address:Ocsigen_config.Socket_type.t ->
  port:int ->
  ssl:bool ->
  filenames:string list Stdlib.ref ->
  client_conn:client_conn ->
  body:Cohttp_lwt.Body.t ->
  connection_closed:unit Lwt.t ->
  Cohttp.Request.t ->
  t
```
```ocaml
val update : 
  ?ssl:bool ->
  ?forward_ip:string list ->
  ?client_conn:client_conn ->
  ?sub_path:string ->
  ?meth:Cohttp.Code.meth ->
  ?get_params_flat:(string * string) list ->
  ?post_data:post_data option ->
  ?cookies_override:string Ocsigen_cookie_map.Map_inner.t ->
  ?full_rewrite:bool ->
  ?uri:Uri.t ->
  t ->
  t
```
```ocaml
val to_cohttp : t -> Cohttp.Request.t
```
```ocaml
val uri : t -> Uri.t
```
```ocaml
val body : t -> Cohttp_lwt.Body.t
```
```ocaml
val address : t -> Ocsigen_config.Socket_type.t
```
```ocaml
val host : t -> string option
```
```ocaml
val meth : t -> Cohttp.Code.meth
```
```ocaml
val port : t -> int
```
```ocaml
val ssl : t -> bool
```
```ocaml
val version : t -> Cohttp.Code.version
```
```ocaml
val query : t -> string option
```
```ocaml
val get_params : t -> (string * string list) list
```
```ocaml
val get_params_flat : t -> (string * string) list
```
```ocaml
val path : t -> string list
```
```ocaml
val path_string : t -> string
```
```ocaml
val sub_path : t -> string list
```
```ocaml
val sub_path_string : t -> string
```
```ocaml
val original_full_path : t -> string list
```
```ocaml
val original_full_path_string : t -> string
```
```ocaml
val header : t -> Ocsigen_header.Name.t -> string option
```
```ocaml
val header_multi : t -> Ocsigen_header.Name.t -> string list
```
```ocaml
val add_header : t -> Ocsigen_header.Name.t -> string -> t
```
```ocaml
val cookies : t -> string Ocsigen_cookie_map.Map_inner.t
```
```ocaml
val files : 
  t ->
  string option ->
  Stdlib.Int64.t option ->
  (string * file_info) list Lwt.t option
```
```ocaml
val post_params : 
  t ->
  string option ->
  Stdlib.Int64.t option ->
  (string * string) list Lwt.t option
```
```ocaml
val client_conn : t -> client_conn
```
The way the client connects to the server (for example, its IP address if connected over the internet).

```ocaml
val client_conn_to_string : t -> string
```
A textual representation of `client_conn` suitable for use in logs.

```ocaml
val forward_ip : t -> string list
```
```ocaml
val content_type : t -> content_type option
```
```ocaml
val request_cache : t -> Polytables.t
```
```ocaml
val tries : t -> int
```
```ocaml
val incr_tries : t -> unit
```
```ocaml
val connection_closed : t -> unit Lwt.t
```
```ocaml
val timeofday : t -> float
```