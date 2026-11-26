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
