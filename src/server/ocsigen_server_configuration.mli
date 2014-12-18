(** An abstract configuration of server configuration *)
type t

(** Maker of server configuration

    @param user user to run server (like www-data)
    @pram group group to run server (like www-data)
    @param ssl_context location of SSL certificate file and SSL key file
    @param threads minimum and maximum of threads pool
    @param ports list of ports to bind the HTTP server
    @param ssl_ports list of ports to bind the HTTPS server

    @return a configuration of server
*)
val make :
  ?user:string ->
  ?group:string ->
  ?ssl_context:(string * string) ->
  ?threads:(int * int) ->
  (Ocsigen_socket.socket_type * int) list ->
  (Ocsigen_socket.socket_type * int) list -> t

(** Accessor for user *)
val user : t -> string option

(** Accessor for group *)
val group : t -> string option

(** Accessor for SSL context *)
val ssl_context : t -> (string * string) option

(** Accessor for minimum and maximum threads pool *)
val threads : t -> (int * int)

(** Accessor for ports to bind the HTTP server *)
val ports : t -> (Ocsigen_socket.socket_type * int) list

(** Accessor for ports to bind the HTTPS server *)
val ssl_ports : t -> (Ocsigen_socket.socket_type * int) list
