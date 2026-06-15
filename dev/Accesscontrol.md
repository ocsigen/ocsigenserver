
# Module `Accesscontrol`

Accesscontrol: Conditional access to some sites

If you want to use this extension with Ocsigen Server's configuration file,

1. have a look at the ` <<a_manual chapter="accesscontrol"|manual page>>`.
2. If you are using Ocsigen Server as a library, use the interface described
3. here. Each of these functions behaves exactly as its configuration file counterpart.
This module belongs to ocamlfind package `ocsigenserver.ext.accesscontrol`.

Example of use (with ` <<a_manual chapter="redirectmod"|Redirectmod>>`):

```ocaml
let _ =
  Ocsigen_server.start
    [ Ocsigen_server.host ~regexp:".*"
        [ Accesscontrol.(
            if_ (not_ ssl)
              [ Redirectmod.run
                  ~redirection:
                    (Redirectmod.create_redirection ~full_url:false
                       ~regexp:"(.* )" "https://yourdomain.org/\\1")
                  () ]
              [ ... ]) ]
    ]
```
```ocaml
type condition
```
```ocaml
val ip : string -> condition
```
```ocaml
val port : int -> condition
```
```ocaml
val ssl : condition
```
```ocaml
val header : name:string -> regexp:string -> condition
```
```ocaml
val method_ : Cohttp.Code.meth -> condition
```
```ocaml
val protocol : Cohttp.Code.version -> condition
```
```ocaml
val path : regexp:string -> condition
```
```ocaml
val and_ : condition list -> condition
```
```ocaml
val or_ : condition list -> condition
```
```ocaml
val not_ : condition -> condition
```
```ocaml
val if_ : 
  condition ->
  Ocsigen_server.instruction list ->
  Ocsigen_server.instruction list ->
  Ocsigen_server.instruction
```
```ocaml
val iffound : Ocsigen_server.instruction list -> Ocsigen_server.instruction
```
```ocaml
val ifnotfound : 
  ?code:string ->
  Ocsigen_server.instruction list ->
  Ocsigen_server.instruction
```
```ocaml
val notfound : Ocsigen_server.instruction
```
```ocaml
val nextsite : Ocsigen_server.instruction
```
```ocaml
val nexthost : Ocsigen_server.instruction
```
```ocaml
val stop : Ocsigen_server.instruction
```
```ocaml
val forbidden : Ocsigen_server.instruction
```
```ocaml
val allow_forward_for : 
  ?check_equal_ip:bool ->
  unit ->
  Ocsigen_server.instruction
```
```ocaml
val allow_forward_proto : unit -> Ocsigen_server.instruction
```
```ocaml
val section : Logs.src
```