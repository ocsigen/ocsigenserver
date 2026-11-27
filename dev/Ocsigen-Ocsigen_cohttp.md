# Module `Ocsigen.Ocsigen_cohttp`

```ocaml
val section : Logs.src
```
```ocaml
exception Ocsigen_http_error of Ocsigen_cookie_map.t * Cohttp.Code.status
```
```ocaml
exception Ext_http_error of Cohttp.Code.status * string option * Cohttp.Header.t option
```
Exception raised by exceptions to describe an HTTP error. It is possible to pass the code of the error, an optional comment, and optionally some headers.

```ocaml
exception Ocsigen_is_dir of Request.t -> Uri.t
```
compute a redirection if path links to a directory

```ocaml
val get_number_of_connected : unit -> int
```
accessor to get number of client (used by eliom monitoring)

```ocaml
val shutdown : float option -> unit
```
Shutdown main loop of server

```ocaml
val max_listening_sockets : Config.socket_type -> int
```
`max_listening_sockets address` is the number of sockets that `listen ~address` opens, at most: two for ``All`, one for IPv4 and one for IPv6, and one otherwise.

```ocaml
type listening
```
Sockets listening for the connections of a server, not served yet.

```ocaml
val listen : 
  ?ssl:(string * string * (bool -> string) option) ->
  address:Config.socket_type ->
  port:int ->
  unit ->
  listening Lwt.t
```
`listen ?ssl ~address ~port ()` opens the sockets of a server on `address` and `port`, with TLS if `ssl` gives a certificate, its key and the function that asks for the password of the key; a Unix-domain socket never has TLS. It fails if a socket cannot be opened, for instance on a port already in use.

```ocaml
val serve : 
  listening ->
  connector:(Request.t -> Response.t Lwt.t) ->
  unit Lwt.t
```
`serve l ~connector` serves the connections of `l`, answering their requests with `connector`, until the server stops.

```ocaml
val service : 
  ?ssl:(string * string * (bool -> string) option) ->
  address:Config.socket_type ->
  port:int ->
  connector:(Request.t -> Response.t Lwt.t) ->
  unit ->
  unit Lwt.t
```
`service ?ssl ~address ~port ~connector ()` is [`listen`](./#val-listen), then [`serve`](./#val-serve).
