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
