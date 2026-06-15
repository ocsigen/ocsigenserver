
# Module `Ocsigen_config.Socket_type`

```ocaml
type t = [ 
  | `All
  | `IPv4 of Unix.inet_addr
  | `IPv6 of Unix.inet_addr
  | `Unix of string
 ]
```
```ocaml
val to_string : t -> string
```