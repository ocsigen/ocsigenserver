(* Raw client for the framing tests: sends to the server of [test.ml], on one
   connection and at once, the requests given as arguments, as "METHOD path",
   the last one with "Connection: close", and prints the bytes of the responses
   as they come. An HTTP client would hide a wrongly framed response: curl, for
   one, opens a new connection when it finds unexpected bytes on the old one. *)

let request ~last i r =
  match String.split_on_char ' ' r with
  | [meth; path] ->
      Printf.sprintf "%s /%s HTTP/1.1\r\nHost: x\r\n%s\r\n" meth path
        (if i = last then "Connection: close\r\n" else "")
  | _ -> invalid_arg ("not METHOD path: " ^ r)

let () =
  let requests = List.tl (Array.to_list Sys.argv) in
  let last = List.length requests - 1 in
  let data = String.concat "" (List.mapi (request ~last) requests) in
  let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.connect fd (Unix.ADDR_UNIX "local.sock");
  Unix.setsockopt_float fd Unix.SO_RCVTIMEO 10.;
  ignore (Unix.write_substring fd data 0 (String.length data));
  let buffer = Bytes.create 4096 in
  let rec copy () =
    match Unix.read fd buffer 0 (Bytes.length buffer) with
    | 0 -> ()
    | n ->
        print_string (Bytes.sub_string buffer 0 n);
        copy ()
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
        print_string "[timeout]\n"
  in
  copy ()
