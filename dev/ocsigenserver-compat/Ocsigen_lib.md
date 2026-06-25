
# Module `Ocsigen_lib`

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
module Let_syntax = Ocsigen_base.Lib.Let_syntax
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
module Tuple3 = Ocsigen_base.Lib.Tuple3
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
module List = Ocsigen_base.Lib.List
```
Improvement of module List

```ocaml
module Clist = Ocsigen_base.Lib.Clist
```
Circular lists

```ocaml
module Int = Ocsigen_base.Lib.Int
```
```ocaml
module String_base = Ocsigen_base.Lib.String_base
```
Improvement of module String

```ocaml
module Url_base = Ocsigen_base.Lib.Url_base
```
```ocaml
val debug : string -> unit
```
```ocaml
val make_cryptographic_safe_string : unit -> string
```
Generate an unique and cryptographically safe random string. It is impossible to guess for other people and will never return twice the same value (with very good probabilities).

```ocaml
module String = Ocsigen_base.Lib.String
```
```ocaml
module Ip_address = Ocsigen_base.Lib.Ip_address
```
```ocaml
module Filename = Ocsigen_base.Lib.Filename
```
```ocaml
module Url = Ocsigen_base.Lib.Url
```