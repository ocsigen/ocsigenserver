
# Module `Ocsigen_header.Accept`

```ocaml
type t = (Mime_type.t * float option * (string * string) list) list
```
```ocaml
val parse : string list -> t
```