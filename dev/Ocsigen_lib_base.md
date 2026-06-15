
# Module `Ocsigen_lib_base`

This module just contains only extensions of the standard library and very basic Ocsigen values and exceptions. Cf. [`Ocsigen_lib`](./Ocsigen_lib.md) for functionality which depends on specific external libraries.

```ocaml
exception Ocsigen_Internal_Error of string
```
```ocaml
exception Input_is_too_large
```
```ocaml
exception Ocsigen_Bad_Request
```
```ocaml
exception Ocsigen_Request_too_long
```
```ocaml
val (>>=) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
```
```ocaml
val (>|=) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t
```
```ocaml
val (<&>) : unit Lwt.t -> unit Lwt.t -> unit Lwt.t
```
```ocaml
val (<?>) : 'a Lwt.t -> 'a Lwt.t -> 'a Lwt.t
```
```ocaml
val (=<<) : ('a -> 'b Lwt.t) -> 'a Lwt.t -> 'b Lwt.t
```
```ocaml
val (=|<) : ('a -> 'b) -> 'a Lwt.t -> 'b Lwt.t
```
```ocaml
module Let_syntax : sig ... end
```
```ocaml
val (!!) : 'a Stdlib.Lazy.t -> 'a
```
```ocaml
val (|>) : 'a -> ('a -> 'b) -> 'b
```
```ocaml
val (@@) : ('a -> 'b) -> 'a -> 'b
```
```ocaml
val id : 'a -> 'a
```
```ocaml
val comp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
```
```ocaml
val curry : (('a * 'b) -> 'c) -> 'a -> 'b -> 'c
```
```ocaml
val uncurry : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
```
```ocaml
module Tuple3 : sig ... end
```
```ocaml
type poly
```
```ocaml
val to_poly : 'a -> poly
```
```ocaml
val from_poly : poly -> 'a
```
```ocaml
type yesnomaybe = 
  | Yes
  | No
  | Maybe
```
```ocaml
type ('a, 'b) leftright = 
  | Left of 'a
  | Right of 'b
```
```ocaml
val advert : string
```
```ocaml
module Option : sig ... end
```
Module Option to compute type `'a option`

```ocaml
module List : sig ... end
```
Improvement of module List

```ocaml
module Clist : sig ... end
```
Circular lists

```ocaml
module Int : sig ... end
```
```ocaml
module String_base : sig ... end
```
Improvement of module String

```ocaml
module Url_base : sig ... end
```
```ocaml
val debug : string -> unit
```