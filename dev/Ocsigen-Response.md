# Module `Ocsigen.Response`

```ocaml
type t
```
A response. The server sends a 500 error instead of a response whose headers or cookies cannot be written as they are: a header name that is not a token (RFC 9110, section 5\.6.2), a header value with a control character other than a tab, a cookie name with `=`, or a cookie name or value with `;` or such a control character.

```ocaml
module Body : sig ... end
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
Like `respond_string`, with `"Error: "` prepended to the body, which is text: the response has `Content-Type: text/plain; charset=utf-8`, unless `headers` gives another one. The default status is ``Internal_server_error`.

```ocaml
val respond_file : 
  ?headers:Cohttp.Header.t ->
  ?status:Http.Status.t ->
  ?if_match:string list ->
  ?if_unmodified_since:string ->
  ?if_none_match:string list ->
  ?if_modified_since:string ->
  ?range:string ->
  ?if_range:string ->
  ?content_type:string ->
  string ->
  t Lwt.t
```
Respond with the content of a file. The content type is `content_type`. By default, it is the one that `Magic_mime` guesses from the name of the file, without the rules of the configuration: Staticmod passes [`Ocsigen_http.Charset_mime.content_type`](./Ocsigen_http-Charset_mime.md#val-content_type). An `ETag` (weak, derived from the file's modification time and size), a `Last-Modified` and an `Accept-Ranges: bytes` header are added. The conditional headers of the request, when passed, are evaluated as RFC 9110 13\.2.2 says. When `If-Match` (with the strong comparison, which the weak `ETag` never passes: only `"*"` matches) or `If-Unmodified-Since` is false, the response is `412 Precondition Failed`. When `If-None-Match` (with the weak comparison) or `If-Modified-Since` (ignored if its date is in the future) is false, the response is `304 Not Modified` with an empty body. `if_match` and `if_none_match` are given as the lines of the header field (see [`Ocsigen_http.Header.Entity_tag.parse_list`](./Ocsigen_http-Header-Entity_tag.md#val-parse_list)). Pass the conditional headers only for a GET or HEAD request whose response is a success: for other methods, a false `If-None-Match` calls for 412 rather than 304, and the preconditions do not apply to an error page (RFC 9110 13\.2.1). If a `range` (the `Range` header) is passed and satisfiable, a single byte range is returned as `206 Partial Content` with a `Content-Range` header (RFC 9110 14\); an unsatisfiable range gives `416`. A passed `if_range` (the `If-Range` header) must be the `Last-Modified` date, unchanged for at least a second, for the range to be honoured: `If-Range` calls for a strong comparison, which the weak `ETag` never passes (RFC 9110 13\.1.5). Pass `range` and `if_range` only for a GET request: ranges are not defined for other methods, and a HEAD request gets the header section of the whole file (RFC 9110 14\.2).

```ocaml
val update : 
  ?response:Cohttp.Response.t ->
  ?body:Body.t ->
  ?cookies:Ocsigen_cookie_map.t ->
  t ->
  t
```
`update ~response ~body ~cookies t` is `t` with the given parts replaced. `update` does not release the former body of `t`: if `body` does not write it, the caller releases it ([`Body.release`](./Ocsigen-Response-Body.md#val-release)).

```ocaml
val of_cohttp : 
  ?cookies:Ocsigen_cookie_map.t ->
  ?release:(unit -> unit Lwt.t) ->
  (Cohttp.Response.t * Cohttp_lwt.Body.t) ->
  t
```
`of_cohttp ~cookies ~release (response, body)` is `response` with `body`, released as [`Body.of_cohttp`](./Ocsigen-Response-Body.md#val-of_cohttp) says.

```ocaml
val sends_body : meth:Cohttp.Code.meth -> t -> bool
```
`sends_body ~meth t` is `true` iff `t`, as the response to a request with method `meth`, carries a body: never for `HEAD`, nor with a 1xx, 204 or 304 status (RFC 9110 6\.4.1, 9\.3.2).

```ocaml
val to_response_expert : 
  meth:Cohttp.Code.meth ->
  version:Cohttp.Code.version ->
  t ->
  Cohttp.Response.t * ('ic -> Lwt_io.output_channel -> unit Lwt.t)
```
Response for `Cohttp_lwt_unix.Server.make_expert`, to a request with method `meth` and HTTP version `version`. Set cookie headers, and the framing headers from the body. The body is written only if [`sends_body`](./#val-sends_body): the response to `HEAD` has the headers of the response to `GET`, without its body. Otherwise the body is released ([`Body.release`](./Ocsigen-Response-Body.md#val-release)). A body of unknown length is sent chunked to an HTTP/1.1 client. To an HTTP/1.0 client, which does not know the chunked coding (RFC 9112 6\.1), a body of unknown length or a chunked one goes out without framing headers and, unless the request is `HEAD`, with `Connection: close`: the end of the connection is the end of the body.

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
