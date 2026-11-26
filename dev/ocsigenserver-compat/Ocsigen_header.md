# Module `Ocsigen_header`

```ocaml
type t = Cohttp.Header.t
```
```ocaml
val of_option : t option -> t
```
```ocaml
module Name = Ocsigen_http.Header.Name
```
```ocaml
val connection_options : t -> string list
```
`connection_options h` is the list of the options of the Connection header of `h`, such as `close`, `keep-alive` or the names of the headers meant for the current connection only, in lowercase (RFC 9110, section 7\.6.1).

```ocaml
val add_vary : t -> Name.t -> t
```
`add_vary h name` adds `name` to the Vary field of `h`, which says that the response depends on the request header `name` (RFC 9110, section 12\.5.5). `h` is unchanged if one of its Vary lines already lists `name`, case-insensitively, or `"*"`. Otherwise, its Vary lines are replaced by a single one that lists their names, then `name`.

```ocaml
module Mime_type = Ocsigen_http.Header.Mime_type
```
```ocaml
module Accept = Ocsigen_http.Header.Accept
```
```ocaml
module Accept_encoding = Ocsigen_http.Header.Accept_encoding
```
```ocaml
module Accept_language = Ocsigen_http.Header.Accept_language
```
```ocaml
module Content_type = Ocsigen_http.Header.Content_type
```
```ocaml
module Entity_tag = Ocsigen_http.Header.Entity_tag
```
Entity tags (RFC 9110, section 8\.8.3).

```ocaml
module Date = Ocsigen_http.Header.Date
```
Dates in header fields (RFC 9110, section 5\.6.7).
