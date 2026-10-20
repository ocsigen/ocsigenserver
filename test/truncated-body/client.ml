(* Client for the truncated body test: sends a POST request announcing a body
   of 100 bytes, raw, as a URL-encoded form or as a multipart form whose
   parameter [a] fills the rest, with only the first [sent] bytes of it, then
   closes its side of the connection, and prints the status line and the body
   of the response. *)

let length = 100

(* The Content-Type header and the body of a request of kind [kind]. *)
let content kind =
  let fill prefix suffix =
    prefix
    ^ String.make (length - String.length prefix - String.length suffix) 'a'
    ^ suffix
  in
  match kind with
  | "raw" -> None, fill "" ""
  | "form" -> Some "application/x-www-form-urlencoded", fill "a=" ""
  | "multipart" ->
      ( Some "multipart/form-data; boundary=X"
      , fill "--X\r\nContent-Disposition: form-data; name=\"a\"\r\n\r\n"
          "\r\n--X--\r\n" )
  | kind -> invalid_arg ("unknown kind " ^ kind)

let () =
  let content_type, body = content Sys.argv.(1) in
  let sent = int_of_string Sys.argv.(2) in
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, 8079));
  Unix.setsockopt_float fd Unix.SO_RCVTIMEO 10.;
  let request =
    "POST /length HTTP/1.1\r\nHost: localhost\r\n"
    ^ Option.fold ~none:""
        ~some:(Printf.sprintf "Content-Type: %s\r\n")
        content_type
    ^ Printf.sprintf "Content-Length: %d\r\n\r\n" length
    ^ String.sub body 0 sent
  in
  ignore (Unix.write_substring fd request 0 (String.length request));
  Unix.shutdown fd Unix.SHUTDOWN_SEND;
  let buffer = Bytes.create 4096 in
  let rec read acc =
    match Unix.read fd buffer 0 (Bytes.length buffer) with
    | 0 -> acc
    | n -> read (acc ^ Bytes.sub_string buffer 0 n)
    | exception Unix.Unix_error _ -> acc
  in
  let response = read "" in
  let status =
    match String.index_opt response '\r' with
    | Some i -> String.sub response 0 i
    | None -> "no response"
  in
  let body =
    match String.rindex_opt response '\n' with
    | Some i -> String.sub response (i + 1) (String.length response - i - 1)
    | None -> ""
  in
  Printf.printf "%s: %s\n" status body
