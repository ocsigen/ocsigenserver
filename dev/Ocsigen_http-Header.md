# Module `Ocsigen_http.Header`

```ocaml
type t = Cohttp.Header.t
```
```ocaml
val of_option : t option -> t
```
```ocaml
module Name : sig ... end
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
module Mime_type : sig ... end
```
```ocaml
module Accept : sig ... end
```
```ocaml
module Accept_encoding : sig ... end
```
```ocaml
module Accept_language : sig ... end
```
```ocaml
module Content_type : sig ... end
```
```ocaml
module Entity_tag : sig ... end
```
Entity tags (RFC 9110, section 8\.8.3).

```ocaml
module Date : sig ... end
```
Dates in header fields (RFC 9110, section 5\.6.7).
