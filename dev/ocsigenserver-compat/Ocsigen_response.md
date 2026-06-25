
# Module `Ocsigen_response`

```ocaml
type t = Ocsigen.Response.t
```
```ocaml
module Body = Ocsigen.Response.Body
```
```ocaml
val make : 
  ?body:Body.t ->
  ?cookies:Ocsigen_cookie_map.t ->
  Cohttp.Response.t ->
  t
```
Make a response from a `Cohttp.Response.t`. Note that the `transfer-encoding` header is not taken into account if it is set to `chunked`, use [`add_header`](./#val-add_header). This is because `Cohttp.Response.make` sets this header by default, which interferes with the transfer-encoding carried by the `body`.

```ocaml
val respond : 
  ?headers:Cohttp.Header.t ->
  status:Http.Status.t ->
  ?body:Body.t ->
  unit ->
  t
```
Like `make` but with an interface similar to `Cohttp_lwt_unix.Server.respond`.

```ocaml
val respond_string : 
  ?headers:Cohttp.Header.t ->
  status:Http.Status.t ->
  body:string ->
  unit ->
  t
```
Like `respond` but with a fixed string body.

```ocaml
val respond_error : 
  ?headers:Cohttp.Header.t ->
  ?status:Http.Status.t ->
  body:string ->
  unit ->
  t
```
Like `respond_string` with `"Error: "` appended to the body. The default status is ``Internal_server_error`.

deprecated Use respond\_string with a ~status argument instead.
```ocaml
val respond_file : 
  ?headers:Cohttp.Header.t ->
  ?status:Http.Status.t ->
  string ->
  t Lwt.t
```
Respond with the content of a file. The content type is guessed using `Magic_mime`.

```ocaml
val update : 
  ?response:Cohttp.Response.t ->
  ?body:Body.t ->
  ?cookies:Ocsigen_cookie_map.t ->
  t ->
  t
```
```ocaml
val of_cohttp : 
  ?cookies:Ocsigen_cookie_map.t ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) ->
  t
```
```ocaml
val to_response_expert : 
  t ->
  Cohttp.Response.t * ('ic -> Lwt_io.output_channel -> unit Lwt.t)
```
Response for `Cohttp_lwt_unix.Server.make_expert`. Set cookie headers.

```ocaml
val response : t -> Cohttp.Response.t
```
Raw response without cookie headers set.

```ocaml
val body : t -> Body.t
```
```ocaml
val status : t -> Cohttp.Code.status
```
```ocaml
val set_status : t -> Cohttp.Code.status -> t
```
```ocaml
val cookies : t -> Ocsigen_cookie_map.t
```
```ocaml
val add_cookies : t -> Ocsigen_cookie_map.t -> t
```
```ocaml
val header : t -> Ocsigen_http.Header.Name.t -> string option
```
```ocaml
val header_multi : t -> Ocsigen_http.Header.Name.t -> string list
```
```ocaml
val add_header : t -> Ocsigen_http.Header.Name.t -> string -> t
```
```ocaml
val add_header_multi : t -> Ocsigen_http.Header.Name.t -> string list -> t
```
```ocaml
val replace_header : t -> Ocsigen_http.Header.Name.t -> string -> t
```
```ocaml
val replace_headers : t -> (Ocsigen_http.Header.Name.t * string) list -> t
```
```ocaml
val remove_header : t -> Ocsigen_http.Header.Name.t -> t
```