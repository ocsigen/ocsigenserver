
# Module `Ocsigen_parseconfig`

```ocaml
val parse_config : ?file:string -> unit -> Xml.xml list list
```
Returns the config file. Use this if you want to read a config file from your own executable. See `Server.exec`.
