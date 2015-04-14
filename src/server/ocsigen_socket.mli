(** Abstraction handling sockets IPv4 and IPv6 *)

(** type of address *)
type socket_type =
  | All
  | IPv4 of Unix.inet_addr
  | IPv6 of Unix.inet_addr

(** make_sockets create socket ready to listen in addr:port
    @param addr type of addresss (All | IPv4 | IPv6)
    @param port port of socket
*)
val make_sockets : socket_type -> int -> Lwt_unix.file_descr list

(** ip_of_sockaddr accessor for ip
    @param A Unix.ADDR_INET value or raise error
*)
val ip_of_sockaddr : Unix.sockaddr -> Unix.inet_addr

(** port_of_sockaddr accessor for port
    @param A Unix.ADDR_INET value or raise error
*)
val port_of_sockaddr : Unix.sockaddr -> int

(** string_of_socket_type cast a Unix.inet_addr in socket_type to a string
    @param A socket_type
*)
val string_of_socket_type : socket_type -> string
