
# Module `Ocsigen.Parseconfig`

Config file parsing. See also module `Extensions.​Configuration`

```ocaml
val parse_config : ?file:string -> unit -> Xml.xml list list
```
Returns the config file. Use this if you want to read a config file from your own executable. See [`Server.exec`](./Ocsigen-Server.md#val-exec).
