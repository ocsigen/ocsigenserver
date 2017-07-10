open Ocsigen_lib

type socket_type =
  | All
  | IPv4 of Unix.inet_addr
  | IPv6 of Unix.inet_addr

(** make_ipv6_socket create a socket on an ipv6 address
 * @param addr address of socket
 * @param port port of socket
 * *)
let make_ipv6_socket addr port =
  let socket = Lwt_unix.socket Unix.PF_INET6 Unix.SOCK_STREAM 0 in
  Lwt_unix.set_close_on_exec socket;
  (* see http://stackoverflow.com/a/14388707/2200717 for more information
   * to why set REUSEADDR on socket *)
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt_unix.setsockopt socket Unix.IPV6_ONLY true;
  Lwt_unix.bind socket (Unix.ADDR_INET (addr, port)) >>= fun () ->
  Lwt.return socket

(** make_ipv4_socket create a socket on an ipv4 address
 * @param addr address of socket
 * @param port port of socket
 * *)
let make_ipv4_socket addr port =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.set_close_on_exec socket;
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt_unix.bind socket (Unix.ADDR_INET (addr, port)) >>= fun () ->
  Lwt.return socket

let make_sockets addr port =
  match addr with
  | All ->
    (* The user didn't specify a protocol in the configuration
       file; we try to open an IPv6 socket (listening to IPv6
       only) if possible and we open an IPv4 socket anyway. This
       corresponds to the net.ipv6.bindv6only=0 behaviour on Linux,
       but is portable and should work with
       net.ipv6.bindv6only=1 as well. *)
    Lwt.catch
      (fun () ->
         make_ipv6_socket Unix.inet6_addr_any port >>= fun s ->
         Lwt.return [s])
      (function
        | Unix.Unix_error
            ((Unix.EAFNOSUPPORT
             | Unix.EPROTONOSUPPORT
             | Unix.EADDRINUSE (* GH issue #104 *)
             ), _, _) ->
          Lwt.return []
        | e ->
          Lwt.fail e) >>= fun ipv6_sockets ->
    make_ipv4_socket Unix.inet_addr_any port >>= fun ipv4_socket ->
    Lwt.return (ipv4_socket :: ipv6_sockets)
  | IPv4 addr ->
    make_ipv4_socket addr port >>= fun s -> Lwt.return [s]
  | IPv6 addr ->
    make_ipv6_socket addr port >>= fun s -> Lwt.return [s]


let ip_of_sockaddr = function
  | Unix.ADDR_INET (ip, port) -> ip
  | _ -> raise (Ocsigen_lib_base.Ocsigen_Internal_Error "ip of unix socket")

let port_of_sockaddr = function
  | Unix.ADDR_INET (ip, port) -> port
  | _ -> raise (Ocsigen_lib_base.Ocsigen_Internal_Error "port of unix socket")

let string_of_socket_type = function
  | All -> Unix.string_of_inet_addr Unix.inet_addr_any
  | IPv4 u -> Unix.string_of_inet_addr u
  | IPv6 u -> Unix.string_of_inet_addr u
