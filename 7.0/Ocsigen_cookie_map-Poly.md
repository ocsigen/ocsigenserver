
# Module `Ocsigen_cookie_map.Poly`

Polymorphic versions of `add` and `remove` to use when we don't need to OUnset (client-side)

```ocaml
val add : 
  path:Ocsigen_lib_base.Url_base.path ->
  string ->
  'a ->
  'a Map_inner.t Map_path.t ->
  'a Map_inner.t Map_path.t
```
```ocaml
val remove : 
  path:Ocsigen_lib_base.Url_base.path ->
  string ->
  'a Map_inner.t Map_path.t ->
  'a Map_inner.t Map_path.t
```