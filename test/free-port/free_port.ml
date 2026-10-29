(* Prints N distinct TCP ports that are free, for tests that start servers:
   several test runs may happen at once on one machine, so a test must not use
   a fixed port.

   Usage: free_port N (the cram tests find free_port.exe in their PATH under
   that name, see ../dune)

   Each port is bound on the IPv4 wildcard address, so that it is not used on
   any IPv4 address, and on the IPv6 wildcard address when the host has IPv6.
   SO_REUSEADDR is not set, so that a port still held by a closed connection
   is not chosen. The sockets are never closed: they hold their port until
   the program exits, so that the N ports are distinct. Another process can
   still take a port before the test binds it, but such a race is unlikely,
   and the server then fails to start (see wait_server in
   ../server-test-helpers.sh). *)

let port_of_socket fd =
  match Unix.getsockname fd with
  | Unix.ADDR_INET (_, port) -> port
  | Unix.ADDR_UNIX _ -> failwith "getsockname: not an Internet socket"

(* [bind_ipv6 port] binds a new socket to [port] of the IPv6 wildcard address,
   for IPv6 only. *)
let bind_ipv6 port =
  match Unix.socket Unix.PF_INET6 Unix.SOCK_STREAM 0 with
  | exception Unix.Unix_error ((Unix.EAFNOSUPPORT | Unix.EPROTONOSUPPORT), _, _)
    ->
      `No_ipv6
  | fd -> (
    match
      Unix.setsockopt fd Unix.IPV6_ONLY true;
      Unix.bind fd (Unix.ADDR_INET (Unix.inet6_addr_any, port))
    with
    | () -> `Bound
    | exception Unix.Unix_error (Unix.EADDRNOTAVAIL, _, _) -> `No_ipv6
    | exception Unix.Unix_error (Unix.EADDRINUSE, _, _) -> `In_use)

(* [choose ~attempts] is a port free on IPv4 and on IPv6. A port free on IPv4
   only stays held, so that it is not chosen again. *)
let rec choose ~attempts =
  if attempts = 0 then failwith "no free port found";
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_any, 0));
  let port = port_of_socket fd in
  match bind_ipv6 port with
  | `Bound | `No_ipv6 -> port
  | `In_use -> choose ~attempts:(attempts - 1)

let () =
  match Array.map int_of_string_opt Sys.argv with
  | [|_; Some n|] when n > 0 ->
      List.init n (fun _ -> choose ~attempts:100)
      |> List.map string_of_int |> String.concat " " |> print_endline
  | _ ->
      prerr_endline "usage: free_port N, with N > 0";
      exit 2
