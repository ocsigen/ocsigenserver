type t =
  {
    user : string option;
    group : string option;
    ssl_context : (string * string) option;
    ports : (Ocsigen_socket.socket_type * int) list;
    ssl_ports : (Ocsigen_socket.socket_type * int) list;
    threads : (int * int)
  }

let make
    ?user
    ?group
    ?ssl_context
    ?(threads=(Ocsigen_config.get_minthreads (),
               Ocsigen_config.get_maxthreads ()))
    ports ssl_ports =
  {
    user;
    group;
    ssl_context;
    ports;
    ssl_ports;
    threads;
  }

let ssl_context { ssl_context; _ } = ssl_context
let user { user; _ } = user
let group { group; _ } = group
let ports { ports; _ } = ports
let ssl_ports { ssl_ports; _ } = ssl_ports
let threads { threads; _ } = threads
