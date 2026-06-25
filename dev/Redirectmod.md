
# Module `Redirectmod`

Redirectmod: HTTP redirections

If you want to use this extension with Ocsigen Server's configuration file, have a look at the [manual page](./redirectmod.md). If you are using Ocsigen Server as a library, use the interface described here. Each of these functions behaves exactly as its configuration file counterpart.

This module belongs to ocamlfind package `ocsigenserver.ext.redirectmod`.

Example of use:

```ocaml
let _ =
   Ocsigen.Server.start
     [ Ocsigen.Server.host ~regexp:".*"
       [ Redirectmod.run
            ~redirection:
              (Redirectmod.create_redirection
                ~temporary:false ~full_url:false ~regexp:"^olddir/(.* )$"
                "https://blahblahblah.org/newdir/\\1")
              ()
       ; Staticmod.run ~dir:"static" ()
       ]
     ]
```
```ocaml
val section : Logs.src
```
```ocaml
type redirection
```
```ocaml
val create_redirection : 
  ?full_url:bool ->
  ?temporary:bool ->
  regexp:string ->
  string ->
  redirection
```
```ocaml
val run : redirection:redirection -> unit -> Ocsigen.Server.instruction
```
`run ~redirection ()` makes it possible to use this extension without configuration file.
