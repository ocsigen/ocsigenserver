
# Module `Deflatemod`

Deflatemod: compress output data

If you want to use this extension with Ocsigen Server's configuration file,

1. have a look at the ` <<a_manual chapter="deflatemod"|manual page>>`.
2. If you are using Ocsigen Server as a library, use the interface described
3. here. Each of these functions behaves exactly as its configuration file counterpart.
This module belongs to ocamlfind package `ocsigenserver.ext.deflatemod`.

Example of use:

```ocaml
let _ =
   Ocsigen_server.start
     [ Ocsigen_server.host ~regexp:".*"
       [ Staticmod.run ~dir:"static" () 
       ; Deflatemod.run
           ~mode:(`Only [ `Type (Some "text", Some "html")
                        ; `Type (Some "text", Some "javascript")
                        ; `Type (Some "text", Some "css")
                        ; `Type (Some "application", Some "javascript")
                        ; `Type (Some "application", Some "x-javascript")
                        ; `Type (Some "application", Some "xhtml+xml")
                        ; `Type (Some "image", Some "svg+xml")
                        ; `Type (Some "application", Some "x-eliom")]) ()
        ]]
```
```ocaml
val set_compress_level : int -> unit
```
```ocaml
val set_buffer_size : int -> unit
```
```ocaml
type filter = [ 
  | `Type of string option * string option
  | `Extension of string
 ]
```
Describes the content to deflate, either using its content type, or file extension

```ocaml
val run : 
  mode:[ `All_but of filter list | `Only of filter list ] ->
  unit ->
  Ocsigen_server.instruction
```
`run ~mode ()` makes it possible to use this extension without configuration file.

```ocaml
val section : Logs.src
```