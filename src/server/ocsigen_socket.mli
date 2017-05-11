(** Abstraction handling sockets IPv4 and IPv6 *)

(** make_sockets create socket ready to listen in addr:port
    @param addr type of addresss (All | IPv4 | IPv6)
    @param port port of socket
*)
val make_sockets :
  Ocsigen_config.socket_type -> int -> Lwt_unix.file_descr list

(** ip_of_sockaddr accessor for ip
    @param A Unix.ADDR_INET value or raise error
*)
val ip_of_sockaddr : Unix.sockaddr -> Unix.inet_addr

(** port_of_sockaddr accessor for port
    @param A Unix.ADDR_INET value or raise error
*)
val port_of_sockaddr : Unix.sockaddr -> int
