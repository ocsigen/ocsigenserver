
# Module `Ocsigen_cache`

Cache. Association tables (from any kind of database) that keep the most recently used values in memory. It is also possible to set a maximum lifetime for data in the cache.

It is based on a structure of doubly linked lists with maximum size, that keeps only the mostly recently used values first, if you call the `up` function each time you use a value. (Insertion, remove and "up" in time 1\). This structure is exported, so that it can be used in other cases.

Not (preemptive) thread safe.

author Vincent Balat
author Raphaël Proust (adding timers)
```ocaml
module Make (A : sig ... end) : sig ... end
```
```ocaml
val clear_all_caches : unit -> unit
```
Clear the contents of all the existing caches

```ocaml
module Dlist : sig ... end
```
Doubly-linked lists with maximum number of entries, and (possibly) limited lifespan for entries.
