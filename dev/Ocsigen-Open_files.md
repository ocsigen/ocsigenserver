# Module `Ocsigen.Open_files`

Limits on the number of files the server process may have open at once.

Each connection is an open file (its socket), so these limits bound the number of simultaneous connections. Many systems keep the soft limit at 1024, for programs that use `select`, while allowing much more: the server can raise it, up to the hard limit.

```ocaml
val limits : unit -> (int * int) option
```
`limits ()` is the soft and the hard limit, `max_int` standing for no limit, or `None` on a system without such limits (Windows).

It raises `Unix.Unix_error` if the system cannot read them.

```ocaml
val raise_soft_limit : int -> (int * int) option
```
`raise_soft_limit n` raises the soft limit to `n`, or to the hard limit if it is lower, and is [`limits`](./#val-limits) then. It never lowers the soft limit. macOS refuses a soft limit above `OPEN_MAX`: there, the soft limit is raised to `OPEN_MAX` at most.

The programs that the server runs inherit the soft limit, and some of them behave badly when it is high, so `n` is best kept to what the server needs.

It raises `Unix.Unix_error` if the system refuses.
