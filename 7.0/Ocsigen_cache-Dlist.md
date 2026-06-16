
# Module `Ocsigen_cache.Dlist`

Doubly-linked lists with maximum number of entries, and (possibly) limited lifespan for entries.

```ocaml
type 'a t
```
```ocaml
type 'a node
```
```ocaml
val create : ?timer:float -> int -> 'a t
```
Create a dlist. It takes the maximum length of the list as parameter. The optional `?timer` parameter sets a maximum lifetime for elements (in seconds).

```ocaml
val add : 'a -> 'a t -> 'a option
```
Adds an element to the list, and possibly returns the element that has been removed if the maximum size was exceeded.

```ocaml
val remove : 'a node -> unit
```
Removes an element from its list. If it is not in a list, it does nothing. If it is in a list, it calls the finaliser, then removes the element. If the finaliser fails with an exception, the element is removed and the exception is raised again.

```ocaml
val up : 'a node -> unit
```
Removes the element from its list without finalising, then adds it as newest.

```ocaml
val newest : 'a t -> 'a node option
```
```ocaml
val oldest : 'a t -> 'a node option
```
```ocaml
val size : 'a t -> int
```
```ocaml
val maxsize : 'a t -> int
```
```ocaml
val value : 'a node -> 'a
```
```ocaml
val get_timer : 'a t -> float option
```
returns the timer of the Dlist

```ocaml
val list_of : 'a node -> 'a t option
```
The list to which the node belongs

```ocaml
val remove_n_oldest : 'a t -> int -> 'a list
```
remove the n oldest values (or less if the list is not long enough) ; returns the list of removed values

```ocaml
val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
```
fold over the elements from the cache starting from the newest to the oldest

```ocaml
val fold_back : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
```
fold over the elements from the cache starting from the oldest to the newest

```ocaml
val move : 'a node -> 'a t -> 'a option
```
Move a node from one dlist to another one, without finalizing. If one value is removed from the destination list (because its maximum size is reached), it is returned (after finalisation).

```ocaml
val set_maxsize : 'a t -> int -> 'a list
```
change the maximum size ; returns the list of removed values, if any.

```ocaml
val set_finaliser_before : ('a node -> unit) -> 'a t -> unit
```
set a function to be called automatically on a piece of data just before it disappears from the list (either by explicit removal or because the maximum size is exceeded)

```ocaml
val get_finaliser_before : 'a t -> 'a node -> unit
```
```ocaml
val add_finaliser_before : ('a node -> unit) -> 'a t -> unit
```
```ocaml
val set_finaliser_after : ('a node -> unit) -> 'a t -> unit
```
set a function to be called automatically on a piece of data just after it disappears from the list (either by explicit removal or because the maximum size is exceeded)

```ocaml
val get_finaliser_after : 'a t -> 'a node -> unit
```
```ocaml
val add_finaliser_after : ('a node -> unit) -> 'a t -> unit
```