
# Class `Make.cache`

`new cache finder ?timer size` creates a cache object where `finder` is the function responsible for retrieving non-cached data, `timer` (if any) is the life span of cached values (in seconds) (values in the cache are removed after their time is up) and `size` is the upper bound to the number of simultaneoulsy cached elements.

Whenever a value is found (using `find` method), it's lifespan is set to `timer` (or not if the cache is not time bounded). If the value was already cached, it's lifespan is reset to `timer`.

Using `timer` allow one to create a cache bounded both in space and time. It is to be noted that real lifespan of values is always slightly greater than `timer`.

```ocaml
method find : A.key -> A.value Lwt.t
```
Find the cached value associated to the key, or binds this value in the cache using the function `finder` passed as argument to `create`, and returns this value

```ocaml
method find_in_cache : A.key -> A.value
```
Find the cached value associated to the key. Raises `Not_found` if the key is not present in the cache

```ocaml
method remove : A.key -> unit
```
```ocaml
method add : A.key -> A.value -> unit
```
```ocaml
method clear : unit -> unit
```
```ocaml
method size : int
```