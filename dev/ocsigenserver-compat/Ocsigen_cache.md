
# Module `Ocsigen_cache`

```ocaml
module Make = Ocsigen_base.Cache.Make
```
```ocaml
val clear_all_caches : unit -> unit
```
Clear the contents of all the existing caches

```ocaml
module Dlist = Ocsigen_base.Cache.Dlist
```
Doubly-linked lists with maximum number of entries, and (possibly) limited lifespan for entries.
