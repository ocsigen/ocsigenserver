
# Module `Extendconfiguration`

Extendconfiguration: More configuration options for Ocsigen Server

If you want to use this extension with Ocsigen Server's configuration file, have a look at the ` <<a_manual chapter="extendconfiguration"|manual page>>`. If you are using Ocsigen Server as a library, use the interface described here. Each of these functions behaves exactly as its configuration file counterpart.

This module belongs to ocamlfind package `ocsigenserver.ext.extendconfiguration`.

Example of use:

```ocaml
let _ =
   Ocsigen_server.start
     [ Ocsigen_server.host ~regexp:".*"
         [ Extendconfiguration.forbidfile ~extensions:["php"] ()
         ; Staticmod.run ~dir:"static" ()
         ]
     ]
```
```ocaml
val followsymlinks : 
  [ `Always | `No | `Owner_match ] ->
  Ocsigen_server.instruction
```
```ocaml
val maxuploadfilesize : int64 option -> Ocsigen_server.instruction
```
```ocaml
val uploaddir : string option -> Ocsigen_server.instruction
```
```ocaml
val listdirs : bool -> Ocsigen_server.instruction
```
```ocaml
val forbidfile : 
  ?files:string list ->
  ?extensions:string list ->
  ?regexps:string list ->
  unit ->
  Ocsigen_server.instruction
```
```ocaml
val hidefile : 
  ?files:string list ->
  ?extensions:string list ->
  ?regexps:string list ->
  unit ->
  Ocsigen_server.instruction
```
```ocaml
val defaultindex : string list -> Ocsigen_server.instruction
```
```ocaml
val contenttype : 
  ?default:string ->
  ?files:(string * string) list ->
  ?extensions:(string * string) list ->
  ?regexps:(string * string) list ->
  unit ->
  Ocsigen_server.instruction
```
```ocaml
val charset : 
  ?default:string ->
  ?files:(string * string) list ->
  ?extensions:(string * string) list ->
  ?regexps:(string * string) list ->
  unit ->
  Ocsigen_server.instruction
```