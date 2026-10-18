(* An upstream server for the reverse proxy tests (port 8069) that answers
   with raw responses, which Ocsigen Server would not send, then closes the
   connection. It serves one connection at a time. *)

let responses =
  [ ( "/http-1.0",
      "HTTP/1.0 200 OK\r\nConnection: close, X-Hop\r\nX-Hop: 1\r\n\
       Keep-Alive: timeout=5\r\nX-End: 1\r\n\r\nbody ended by the connection\n"
    );
    ( "/length",
      "HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Length: 3\r\n\r\nok\n" )
  ]

let rec skip_headers ic =
  match input_line ic with "" | "\r" -> () | _ -> skip_headers ic

let answer fd =
  let ic = Unix.in_channel_of_descr fd
  and oc = Unix.out_channel_of_descr fd in
  let path =
    match String.split_on_char ' ' (input_line ic) with
    | _ :: path :: _ -> path
    | _ -> ""
  in
  skip_headers ic;
  output_string oc
    (Option.value ~default:"HTTP/1.0 404 Not Found\r\n\r\n"
       (List.assoc_opt path responses));
  close_out oc

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 8069));
  Unix.listen socket 16;
  while true do
    let fd, _ = Unix.accept socket in
    try answer fd
    with End_of_file | Sys_error _ -> Unix.close fd
  done
