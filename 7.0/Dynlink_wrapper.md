
# Module `Dynlink_wrapper`

```ocaml
exception Error of Dynlink.error
```
```ocaml
val loadfile : string -> unit
```
```ocaml
val error_message : Dynlink.error -> string
```
```ocaml
val allow_unsafe_modules : bool -> unit
```
```ocaml
val prohibit : string list -> unit
```
```ocaml
val is_native : bool
```