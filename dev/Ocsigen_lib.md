
# Module `Ocsigen_lib`

This module contains some auxiliaries for the Ocsigenserver. In contrast to [`Ocsigen_lib_base`](./Ocsigen_lib_base.md), the function may also refer to libraries other than the standard library.

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
type poly = Ocsigen_lib_base.poly
```
```ocaml
val to_poly : 'a -> poly
```
```ocaml
val from_poly : poly -> 'a
```
```ocaml
type yesnomaybe = Ocsigen_lib_base.yesnomaybe = 
  | Yes
  | No
  | Maybe
```
```ocaml
val advert : string
```
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
```ocaml
val make_cryptographic_safe_string : unit -> string
```
Generate an unique and cryptographically safe random string. It is impossible to guess for other people and will never return twice the same value (with very good probabilities).

```ocaml
module String : module type of String_base
```
Improvement of module String

```ocaml
module Ip_address : sig ... end
```
```ocaml
module Filename : sig ... end
```
```ocaml
module Url : sig ... end
```