
# Module `Ocsigen_lib.Let_syntax`

```ocaml
val return : 'a -> 'a Lwt.t
```
```ocaml
val map : 'a Lwt.t -> f:('a -> 'b) -> 'b Lwt.t
```
```ocaml
val bind : 'a Lwt.t -> f:('a -> 'b Lwt.t) -> 'b Lwt.t
```
```ocaml
val both : 'a Lwt.t -> 'b Lwt.t -> ('a * 'b) Lwt.t
```
```ocaml
module Open_on_rhs : sig ... end
```