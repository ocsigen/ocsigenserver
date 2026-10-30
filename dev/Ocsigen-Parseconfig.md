# Module `Ocsigen.Parseconfig`

Config file parsing. See also module [`Ocsigen.Extensions.Configuration`](./Ocsigen-Extensions-Configuration.md)

```ocaml
val parse_config : ?file:string -> unit -> Xml.xml list list
```
Returns the config file. Use this if you want to read a config file from your own executable. See [`Server.exec`](./Ocsigen-Server.md#val-exec). Raises `Config.Config_file_error` on an error, such as a file that cannot be opened or is not well-formed XML.
