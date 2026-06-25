
# Module `Accesscontrol`

Accesscontrol: Conditional access to some sites

If you want to use this extension with Ocsigen Server's configuration file,

1. have a look at the [manual page](./accesscontrol.md).
2. If you are using Ocsigen Server as a library, use the interface described
3. here. Each of these functions behaves exactly as its configuration file counterpart.
This module belongs to ocamlfind package `ocsigenserver.ext.accesscontrol`.

Example of use (with [Redirectmod](./redirectmod.md)):

```ocaml
let _ =
  Ocsigen.Server.start
    [ Ocsigen.Server.host ~regexp:".*"
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
  Ocsigen.Server.instruction list ->
  Ocsigen.Server.instruction list ->
  Ocsigen.Server.instruction
```
```ocaml
val iffound : Ocsigen.Server.instruction list -> Ocsigen.Server.instruction
```
```ocaml
val ifnotfound : 
  ?code:string ->
  Ocsigen.Server.instruction list ->
  Ocsigen.Server.instruction
```
```ocaml
val notfound : Ocsigen.Server.instruction
```
```ocaml
val nextsite : Ocsigen.Server.instruction
```
```ocaml
val nexthost : Ocsigen.Server.instruction
```
```ocaml
val stop : Ocsigen.Server.instruction
```
```ocaml
val forbidden : Ocsigen.Server.instruction
```
```ocaml
val allow_forward_for : 
  ?check_equal_ip:bool ->
  unit ->
  Ocsigen.Server.instruction
```
```ocaml
val allow_forward_proto : unit -> Ocsigen.Server.instruction
```
```ocaml
val section : Logs.src
```