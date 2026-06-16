
# Module `Staticmod`

Staticmod: serve static files

If you want to use this extension with Ocsigen Server's configuration file, have a look at the [manual page](./staticmod.md). If you are using Ocsigen Server as a library, use the interface described here.

This module belongs to ocamlfind package `ocsigenserver.ext.staticmod`.

Example of use:

```ocaml
let _ =
   Ocsigen_server.start
     [ Ocsigen_server.host ~regexp:".*" [ Staticmod.run ~dir:"static" () ]]
```
```ocaml
val run : 
  ?dir:string ->
  ?regexp:string ->
  ?dest:string ->
  ?code:string ->
  ?cache:int ->
  ?root:string ->
  unit ->
  Ocsigen_server.instruction
```
Run static mod on a specific directory. Call this if you want to run Ocsigen Server without configuration file. The optional parameter correspond to the options of the configuration file described [here](./staticmod.md).

```ocaml
val section : Logs.src
```