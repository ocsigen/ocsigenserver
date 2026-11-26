# Module `Header.Entity_tag`

Entity tags (RFC 9110, section 8\.8.3).

```ocaml
type t = private {
  weak : bool;
  opaque : string;
}
```
`opaque` is the tag without its quotes. It only has the characters that RFC 9110 allows in it (etagc): no space, control character or double quote, so that [`to_string`](./#val-to_string) always gives a valid entity tag.

```ocaml
val make : weak:bool -> string -> t
```
`make ~weak opaque` is the entity tag `opaque`, weak if `weak`. Raises `Invalid_argument` if `opaque` has a character that an entity tag cannot contain.

```ocaml
val to_string : t -> string
```
`to_string t` is `t` as written in a header, such as `W/"1-2"`.

```ocaml
val parse_list : string list -> [ `Any | `Tags of t list ]
```
`parse_list values` is the value of an `If-Match` or `If-None-Match` header field written on the lines `values`: ``Any` when it is just `"*"`, otherwise the list of its entity tags. Elements that are not valid entity tags are ignored.

```ocaml
val weak_equal : t -> t -> bool
```
`weak_equal a b` is the weak comparison of `a` and `b`: their opaque tags are equal, whether they are weak or not (section 8\.8.3.2).

```ocaml
val strong_equal : t -> t -> bool
```
`strong_equal a b` is the strong comparison of `a` and `b`: they are not weak and their opaque tags are equal.
