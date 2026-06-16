
# Module `Ocsigen_cache.Make`


## Parameters

```ocaml
module A : sig ... end
```

## Signature

```ocaml
class cache : (A.key -> A.value Lwt.t) -> ?timer:float -> int -> object ... end
```
`new cache finder ?timer size` creates a cache object where `finder` is the function responsible for retrieving non-cached data, `timer` (if any) is the life span of cached values (in seconds) (values in the cache are removed after their time is up) and `size` is the upper bound to the number of simultaneoulsy cached elements.
