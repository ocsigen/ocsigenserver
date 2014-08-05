(** Abstraction handling sockets IPv4 and IPv6 *)

(** Type of address *)
type socket_type =
  | All
  | IPv4 of Unix.inet_addr
  | IPv6 of Unix.inet_addr

(** [make_sockets addr port] create sockets ready to listen to ["addr:port"].
*)
val make_sockets : socket_type -> int -> Lwt_unix.file_descr list

(** [ip_of_sockaddr s] returns its ip address.
    [s] should always be a [Unix.ADDR_INET].
*)
val ip_of_sockaddr : Unix.sockaddr -> Unix.inet_addr

(** [port_of_sockaddr s] returns its port.
    [s] should always be a [Unix.ADDR_INET].
*)
val port_of_sockaddr : Unix.sockaddr -> int

(** [string_of_socket_type s_ty] casts the [Unix.ADDR_INET] in [s_ty] to a string.
*)
val string_of_socket_type : socket_type -> string

(** [socket_type_of_string] cast a string ["addr:port"] to a socket type
    @param str string in format "addr:port"
*)
val socket_type_of_string : string -> socket_type
