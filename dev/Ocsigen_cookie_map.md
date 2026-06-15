
# Module `Ocsigen_cookie_map`

```ocaml
module Map_path : Stdlib.Map.S with type key := Ocsigen_lib_base.Url_base.path
```
This type of maps is used to store cookie values for each path. The key has type Url.path option: it is for the path (default: root of the site).

```ocaml
module Map_inner : Stdlib.Map.S with type key := string
```
```ocaml
type cookie = 
  | OSet of float option * string * bool
  | OUnset
```
Type used for cookies to set. The float option is the timestamp for the expiration date. The string is the value. If the bool is true and the protocol is https, the cookie will be secure (will ask the browser to send it only through secure connections).

```ocaml
type t = cookie Map_inner.t Map_path.t
```
```ocaml
val empty : t
```
```ocaml
val add : path:Ocsigen_lib_base.Url_base.path -> string -> cookie -> t -> t
```
`add ~path c v m` adds the cookie `c` to `m`.

If the cookie is already bound, the previous binding disappear.

```ocaml
val add_multi : t -> t -> t
```
`add_multi new old` adds the cookies from `new` to `old`. If cookies are already bound in oldcookies, the previous binding disappear.

```ocaml
val remove : path:Ocsigen_lib_base.Url_base.path -> string -> t -> t
```
`remove c cookie_table` removes the cookie `c` from `m`.

Warning: it is not equivalent to `add ... OUnset ...`).

```ocaml
module Poly : sig ... end
```
Polymorphic versions of `add` and `remove` to use when we don't need to OUnset (client-side)
