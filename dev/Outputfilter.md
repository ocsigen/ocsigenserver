# Module `Outputfilter`

Outputfilter: Rewrite some part of the output

If you want to use this extension with Ocsigen Server's configuration file, have a look at the [manual page](./outputfilter.md). If you are using Ocsigen Server as a library, use the interface described here. Each of these functions behaves exactly as its configuration file counterpart.

This module belongs to ocamlfind package `ocsigenserver.ext.outputfilter`.

See an example of use on the API documentation of [`Revproxy`](./Revproxy.md).

```ocaml
val run : 
  mode:
    [ `Rewrite of Ocsigen_http.Header.Name.t * string * string
    | `Add of Ocsigen_http.Header.Name.t * string * bool option
    | `Code of Cohttp.Code.status ] ->
  unit ->
  Ocsigen.Server.instruction
```
`run ~mode ()` makes it possible to use this extension without configuration file. It changes the response of the instructions before it:

- ``Rewrite (name, regexp, dest)` replaces every match of `regexp` in the values of header `name` with `dest`, where `\1` to `\9` stand for the groups of `regexp`;
- ``Add (name, value, replace)` adds header `name`: if the response has one already, `Some true` replaces it, `Some false` adds another one, and `None` leaves it;
- ``Code status` sets the status of the response.
