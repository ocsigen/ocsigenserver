
# Module `Polytables`

Polymorphic tables (using Map)

author Vincent Balat
author Jérôme Vouillon
Warning: this module is not thread safe\!

```ocaml
type 'a key
```
The type of key for a piece of data of type 'a

```ocaml
type t
```
The type of tables

```ocaml
val create : unit -> t
```
creates a new table

```ocaml
val make_key : unit -> 'a key
```
create a new key for each data you want to save

```ocaml
val set : table:t -> key:'a key -> value:'a -> unit
```
`set t k v` associates `v` to `k` in `t`

```ocaml
val get : table:t -> key:'a key -> 'a
```
`get t k` returns the current binding of `k` in `t` or raises `Not_found`

```ocaml
val remove : table:t -> key:'a key -> unit
```
`remove t k` remove the current binding of `k` in `t` if it exists

```ocaml
val clear : table:t -> unit
```
`clear t` remove all data from t
