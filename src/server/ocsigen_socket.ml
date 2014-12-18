type socket_type =
  | All
  | IPv4 of Unix.inet_addr
  | IPv6 of Unix.inet_addr

let make_ipv6_socket addr port =
  let socket = Lwt_unix.socket Unix.PF_INET6 Unix.SOCK_STREAM 0 in
  Lwt_unix.set_close_on_exec socket;
  (* see http://stackoverflow.com/a/14388707/2200717 for more information
     to why set REUSEADDR on socket *)
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt_unix.setsockopt socket Unix.IPV6_ONLY true;
  Lwt_unix.bind socket (Unix.ADDR_INET (addr, port));
  socket

let make_ipv4_socket addr port =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.set_close_on_exec socket;
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt_unix.bind socket (Unix.ADDR_INET (addr, port));
  socket

let make_sockets addr port =
  match addr with
  | All ->
    (* The user didn't specify a protocol in the configuration
       file; we try to open an IPv6 socket (listening to IPv6
       only) if possible and we open an IPv4 socket anyway. This
       corresponds to the net.ipv6.bindv6only=0 behaviour on Linux,
       but is portable and should work with
       net.ipv6.bindv6only=1 as well. *)
    let ipv6_socket =
      try [make_ipv6_socket Unix.inet6_addr_any port]
      with Unix.Unix_error
          ((Unix.EAFNOSUPPORT | Unix.EPROTONOSUPPORT),
           _, _) -> []
    in
    (make_ipv4_socket Unix.inet_addr_any port)::ipv6_socket
  | IPv4 addr ->
    [make_ipv4_socket addr port]
  | IPv6 addr ->
    [make_ipv6_socket addr port]


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

let socket_type_of_string =
  let all_ipv6 = Netstring_pcre.regexp "^\\[::\\]:([0-9]+)$" in
  let all_ipv4 = Netstring_pcre.regexp "^\\*:([0-9]+)$" in
  let single_ipv6 = Netstring_pcre.regexp "^\\[([0-9A-Fa-f.:]+)\\]:([0-9]+)+" in
  let single_ipv4 = Netstring_pcre.regexp "^([0-9.]+):([0-9]+)$" in
  let aux str =
    let match_addr regexp = Netstring_pcre.string_match regexp str 0 in
    let get_addr regexp = Netstring_pcre.matched_group regexp 1 str in
    match match_addr all_ipv6 with
    | Some r -> IPv6 Unix.inet6_addr_any
    | None ->
      match match_addr all_ipv4 with
      | Some r -> IPv4 Unix.inet_addr_any
      | None ->
        match match_addr single_ipv6 with
        | Some r -> IPv6 (Unix.inet_addr_of_string (get_addr r))
        | None ->
          match match_addr single_ipv4 with
          | Some r -> IPv4 (Unix.inet_addr_of_string (get_addr r))
          | None -> All
  in aux
