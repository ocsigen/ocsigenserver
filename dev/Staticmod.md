# Module `Staticmod`

Staticmod: serve static files

If you want to use this extension with Ocsigen Server's configuration file, have a look at the [manual page](./staticmod.md). If you are using Ocsigen Server as a library, use the interface described here.

This module belongs to ocamlfind package `ocsigenserver.ext.staticmod`.

Example of use:

```ocaml
let _ =
   Ocsigen.Server.start
     [ Ocsigen.Server.host ~regexp:".*" [ Staticmod.run ~dir:"static" () ]]
```
```ocaml
val run : 
  ?dir:string ->
  ?regexp:string ->
  ?dest:string ->
  ?code:string ->
  ?cache:int ->
  ?root:string ->
  ?hide_dotfiles:bool ->
  unit ->
  Ocsigen.Server.instruction
```
Run static mod on a specific directory. Call this if you want to run Ocsigen Server without configuration file. The optional parameter correspond to the options of the configuration file described [here](./staticmod.md). When `hide_dotfiles` is `true` (the default), the paths with a segment that starts with a dot (such as `.git/config` or `.env`) are neither served nor listed, except under `/.well-known/`.

```ocaml
val section : Logs.src
```
