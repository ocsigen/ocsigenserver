
# Module `Ocsigen_parseconfig`

Config file parsing. See also module `Ocsigen_extensions.​Configuration`

```ocaml
val parse_config : ?file:string -> unit -> Xml.xml list list
```
Returns the config file. Use this if you want to read a config file from your own executable. See [`Ocsigen_server.exec`](./Ocsigen_server.md#val-exec).
