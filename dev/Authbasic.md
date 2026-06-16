
# Module `Authbasic`

Authbasic: Basic HTTP authentication

If you want to use this extension with Ocsigen Server's configuration file, have a look at the [manual page](./authbasic.md). If you are using Ocsigen Server as a library, use the interface described here. Each of these functions behaves exactly as its configuration file counterpart.

This module belongs to ocamlfind package `ocsigenserver.ext.authbasic`.

Example of use:

```ocaml
let _ =
   Ocsigen_server.start
     [ Ocsigen_server.host ~regexp:".*"
       [ Authbasic.run ~realm:"test"
            ~auth:(fun u p -> Lwt.return (u = "theuser" && p = "thepassword"))
            () 
       ; Staticmod.run ~dir:"static" () ]]
```
This module implements Basic HTTP Authentication as described in [RFC 2617](http://www.ietf.org/rfc/rfc2617.txt). It can be used to add an authentication layer to sites with no built-in authentication (e.g. static files). Beware, passwords are transmitted in cleartext with this scheme, so the medium should be secured somehow (by e.g. SSL).

This module implements only the HTTP-related part of the protocol, and is meant to be extended with various authentication schemes. A very naive one (authentication with a single user/password, given in the configuration file) is provided.

```ocaml
val section : Logs.src
```
```ocaml
type auth = string -> string -> bool Lwt.t
```
```ocaml
val register_basic_authentication_method : (Xml.xml -> auth) -> unit
```
This function registers an authentication plugin: it adds a new parser to the list of available authentication schemes.

This is only applied if you are running the server with an XML configuration file. Use the realm, auth variables otherwise.

A parser takes as argument an XML tree (corresponding to the first son of an \<authbasic\> element in the configuration file) and returns an authentication function `f`. `f` will be called for each request with the supplied user and password and should return (cooperatively) a boolean telling whether access is granted or not. Exceptions are handled the same way as for extension parsers.

The \<authbasic\> element must have a *realm* attribute, giving some identifier to the resource which is protected (several resources on the same hostname can share the same realm). This gives a general customization scheme "for free" from the point of view of plugin developers and is totally transparent to the plugin.

```ocaml
val run : realm:string -> auth:auth -> unit -> Ocsigen_server.instruction
```
`run ~realm ~auth ()` makes it possible to use this extension without configuration file.
