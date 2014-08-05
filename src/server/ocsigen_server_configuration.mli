type t

val make :
  ?user:string ->
  ?group:string ->
  ?ssl_context:(string * string) ->
  ?threads:(int * int) ->
  (Ocsigen_socket.socket_type * int) list ->
  (Ocsigen_socket.socket_type * int) list -> t

val user : t -> string option
val group : t -> string option
val ssl_context : t -> (string * string) option
val threads : t -> (int * int)
val ports : t -> (Ocsigen_socket.socket_type * int) list
val ssl_ports : t -> (Ocsigen_socket.socket_type * int) list
