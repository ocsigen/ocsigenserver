# Module `Connection_guard.Per_address`

Number of simultaneous connections from a single client.

```ocaml
val acquire : limit:int -> Ipaddr.t -> bool
```
`acquire ~limit ip` counts a new connection from `ip`, unless `limit` connections from it are already open, in which case it is `false`. Addresses are grouped as clients are: an IPv6 address counts for its /64 network, the usual allocation to a single site, and an IPv4-mapped IPv6 address for its IPv4 address. A `limit` of `0` or less means no limit.

```ocaml
val release : Ipaddr.t -> unit
```
`release ip` counts the end of a connection from `ip` that [`acquire`](./#val-acquire) accepted.

```ocaml
val count : Ipaddr.t -> int
```
`count ip` is the number of connections from the group of `ip`.
