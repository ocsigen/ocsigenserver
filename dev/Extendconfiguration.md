
# Module `Extendconfiguration`

Extendconfiguration: More configuration options for Ocsigen Server

If you want to use this extension with Ocsigen Server's configuration file, have a look at the [manual page](./extendconfiguration.md). If you are using Ocsigen Server as a library, use the interface described here. Each of these functions behaves exactly as its configuration file counterpart.

This module belongs to ocamlfind package `ocsigenserver.ext.extendconfiguration`.

Example of use:

```ocaml
let _ =
   Ocsigen.Server.start
     [ Ocsigen.Server.host ~regexp:".*"
         [ Extendconfiguration.forbidfile ~extensions:["php"] ()
         ; Staticmod.run ~dir:"static" ()
         ]
     ]
```
```ocaml
val followsymlinks : 
  [ `Always | `No | `Owner_match ] ->
  Ocsigen.Server.instruction
```
```ocaml
val maxuploadfilesize : int64 option -> Ocsigen.Server.instruction
```
```ocaml
val uploaddir : string option -> Ocsigen.Server.instruction
```
```ocaml
val listdirs : bool -> Ocsigen.Server.instruction
```
```ocaml
val forbidfile : 
  ?files:string list ->
  ?extensions:string list ->
  ?regexps:string list ->
  unit ->
  Ocsigen.Server.instruction
```
```ocaml
val hidefile : 
  ?files:string list ->
  ?extensions:string list ->
  ?regexps:string list ->
  unit ->
  Ocsigen.Server.instruction
```
```ocaml
val defaultindex : string list -> Ocsigen.Server.instruction
```
```ocaml
val contenttype : 
  ?default:string ->
  ?files:(string * string) list ->
  ?extensions:(string * string) list ->
  ?regexps:(string * string) list ->
  unit ->
  Ocsigen.Server.instruction
```
```ocaml
val charset : 
  ?default:string ->
  ?files:(string * string) list ->
  ?extensions:(string * string) list ->
  ?regexps:(string * string) list ->
  unit ->
  Ocsigen.Server.instruction
```