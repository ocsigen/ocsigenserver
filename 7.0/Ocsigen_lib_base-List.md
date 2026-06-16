
# Module `Ocsigen_lib_base.List`

Improvement of module List

```ocaml
type !'a t = 'a list = 
  | []
  | :: of 'a * 'a list
```
```ocaml
val length : 'a list -> int
```
```ocaml
val compare_lengths : 'a list -> 'b list -> int
```
```ocaml
val compare_length_with : 'a list -> int -> int
```
```ocaml
val is_empty : 'a list -> bool
```
```ocaml
val cons : 'a -> 'a list -> 'a list
```
```ocaml
val singleton : 'a -> 'a list
```
```ocaml
val hd : 'a list -> 'a
```
```ocaml
val tl : 'a list -> 'a list
```
```ocaml
val nth : 'a list -> int -> 'a
```
```ocaml
val nth_opt : 'a list -> int -> 'a option
```
```ocaml
val rev : 'a list -> 'a list
```
```ocaml
val init : int -> (int -> 'a) -> 'a list
```
```ocaml
val append : 'a list -> 'a list -> 'a list
```
```ocaml
val rev_append : 'a list -> 'a list -> 'a list
```
```ocaml
val concat : 'a list list -> 'a list
```
```ocaml
val flatten : 'a list list -> 'a list
```
```ocaml
val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
```
```ocaml
val compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int
```
```ocaml
val iter : ('a -> unit) -> 'a list -> unit
```
```ocaml
val iteri : (int -> 'a -> unit) -> 'a list -> unit
```
```ocaml
val map : ('a -> 'b) -> 'a list -> 'b list
```
```ocaml
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
```
```ocaml
val rev_map : ('a -> 'b) -> 'a list -> 'b list
```
```ocaml
val filter_map : ('a -> 'b option) -> 'a list -> 'b list
```
```ocaml
val concat_map : ('a -> 'b list) -> 'a list -> 'b list
```
```ocaml
val fold_left_map : 
  ('acc -> 'a -> 'acc * 'b) ->
  'acc ->
  'a list ->
  'acc * 'b list
```
```ocaml
val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc
```
```ocaml
val fold_right : ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc
```
```ocaml
val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
```
```ocaml
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
```
```ocaml
val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
```
```ocaml
val fold_left2 : 
  ('acc -> 'a -> 'b -> 'acc) ->
  'acc ->
  'a list ->
  'b list ->
  'acc
```
```ocaml
val fold_right2 : 
  ('a -> 'b -> 'acc -> 'acc) ->
  'a list ->
  'b list ->
  'acc ->
  'acc
```
```ocaml
val for_all : ('a -> bool) -> 'a list -> bool
```
```ocaml
val exists : ('a -> bool) -> 'a list -> bool
```
```ocaml
val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
```
```ocaml
val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
```
```ocaml
val mem : 'a -> 'a list -> bool
```
```ocaml
val memq : 'a -> 'a list -> bool
```
```ocaml
val find : ('a -> bool) -> 'a list -> 'a
```
```ocaml
val find_opt : ('a -> bool) -> 'a list -> 'a option
```
```ocaml
val find_index : ('a -> bool) -> 'a list -> int option
```
```ocaml
val find_map : ('a -> 'b option) -> 'a list -> 'b option
```
```ocaml
val find_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b option
```
```ocaml
val filter : ('a -> bool) -> 'a list -> 'a list
```
```ocaml
val find_all : ('a -> bool) -> 'a list -> 'a list
```
```ocaml
val filteri : (int -> 'a -> bool) -> 'a list -> 'a list
```
```ocaml
val take : int -> 'a list -> 'a list
```
```ocaml
val drop : int -> 'a list -> 'a list
```
```ocaml
val take_while : ('a -> bool) -> 'a list -> 'a list
```
```ocaml
val drop_while : ('a -> bool) -> 'a list -> 'a list
```
```ocaml
val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
```
```ocaml
val partition_map : 
  ('a -> ('b, 'c) Stdlib.Either.t) ->
  'a list ->
  'b list * 'c list
```
```ocaml
val assoc : 'a -> ('a * 'b) list -> 'b
```
```ocaml
val assoc_opt : 'a -> ('a * 'b) list -> 'b option
```
```ocaml
val assq : 'a -> ('a * 'b) list -> 'b
```
```ocaml
val assq_opt : 'a -> ('a * 'b) list -> 'b option
```
```ocaml
val mem_assoc : 'a -> ('a * 'b) list -> bool
```
```ocaml
val mem_assq : 'a -> ('a * 'b) list -> bool
```
```ocaml
val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
```
```ocaml
val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
```
```ocaml
val split : ('a * 'b) list -> 'a list * 'b list
```
```ocaml
val combine : 'a list -> 'b list -> ('a * 'b) list
```
```ocaml
val sort : ('a -> 'a -> int) -> 'a list -> 'a list
```
```ocaml
val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
```
```ocaml
val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
```
```ocaml
val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list
```
```ocaml
val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
```
```ocaml
val to_seq : 'a list -> 'a Stdlib.Seq.t
```
```ocaml
val of_seq : 'a Stdlib.Seq.t -> 'a list
```
```ocaml
val map_filter : ('a -> 'b option) -> 'a list -> 'b list
```
```ocaml
val last : 'a list -> 'a
```
```ocaml
val assoc_remove : 'a -> ('a * 'b) list -> 'b * ('a * 'b) list
```
```ocaml
val remove_first_if_any : 'a -> 'a list -> 'a list
```
```ocaml
val remove_first_if_any_q : 'a -> 'a list -> 'a list
```
```ocaml
val remove_first : 'a -> 'a list -> 'a list
```
```ocaml
val remove_first_q : 'a -> 'a list -> 'a list
```
```ocaml
val remove_all : 'a -> 'a list -> 'a list
```
```ocaml
val remove_all_q : 'a -> 'a list -> 'a list
```
```ocaml
val remove_all_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
```
```ocaml
val remove_all_assoc_q : 'a -> ('a * 'b) list -> ('a * 'b) list
```
```ocaml
val is_prefix : 'a list -> 'a list -> bool
```
```ocaml
val chop : int -> 'a list -> 'a list
```
```ocaml
val split_at : int -> 'a list -> 'a list * 'a list
```