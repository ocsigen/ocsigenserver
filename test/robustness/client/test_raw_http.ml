(* Tests of the pure parts of Raw_http: rendering and status extraction. The
   network parts are exercised by the cram tests of the parent directory. *)

let show title outcome =
  Printf.printf "== %s\n%s" title (Raw_http.render outcome)

let show_status title outcome =
  Printf.printf "== %s\n%s" title (Raw_http.render ~status_only:true outcome)

let response =
  "HTTP/1.1 200 OK\r\nDate: Sat, 17 Oct 2026 10:00:00 GMT\r\nETag: W/\"1-2\"\r\nlast-modified: Sat, 17 Oct 2026 09:00:00 GMT\r\nContent-Length: 5\r\n\r\nhello"

let () =
  show "headers that vary are removed, CRLF becomes LF"
    {Raw_http.received = response; ending = Closed};
  Printf.printf "== validators kept, Date still removed\n%s"
    (Raw_http.render ~validators:true
       {Raw_http.received = response; ending = Closed});
  show "control and non-ASCII bytes are escaped"
    {received = "a\x00b\x1bc\xe9\td\re\n"; ending = Idle};
  show "empty answer" {received = ""; ending = Reset};
  show "a final line end does not add an empty line"
    {received = "x\r\n"; ending = Overflow};
  show_status "status only; a status line glued to a body is not seen"
    {received = response ^ "HTTP/1.1 400 Bad Request\r\n\r\n"; ending = Closed};
  Printf.printf "== status codes\n%s\n"
    (String.concat " "
       (List.map string_of_int
          (Raw_http.status_codes
             (response ^ "\r\nHTTP/1.1 404 Not Found\r\nHTTP/1.0 bad\r\n"))))

let framing_name = function
  | Raw_http.Complete {close = false} -> "complete"
  | Complete {close = true} -> "complete, close"
  | Incomplete -> "incomplete"
  | Unframed -> "unframed"

let () =
  print_endline "== framing";
  List.iter
    (fun (title, received) ->
       Printf.printf "%s: %s\n" title (framing_name (Raw_http.framing received)))
    [ "nothing", ""
    ; "content-length", response
    ; "content-length, content missing", String.sub response 0 100
    ; "head section not finished", "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n"
    ; "two responses", response ^ response
    ; "extra bytes", response ^ "x"
    ; ( "304 without content"
      , "HTTP/1.1 304 Not Modified\r\nContent-Length: 6\r\n\r\n" )
    ; "100 alone", "HTTP/1.1 100 Continue\r\n\r\n"
    ; ( "100 then 204"
      , "HTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 204 No Content\r\n\r\n" )
    ; "101", "HTTP/1.1 101 Switching Protocols\r\nUpgrade: x\r\n\r\n"
    ; "no reason phrase", "HTTP/1.1 204\r\n\r\n"
    ; "HTTP/1.0", "HTTP/1.0 204 No Content\r\n\r\n"
    ; ( "HTTP/1.0 with keep-alive"
      , "HTTP/1.0 204 No Content\r\nConnection: Keep-Alive\r\n\r\n" )
    ; ( "transfer-encoding gzip alone"
      , "HTTP/1.1 200 OK\r\nTransfer-Encoding: gzip\r\n\r\nx" )
    ; ( "connection: close"
      , "HTTP/1.1 204 No Content\r\nConnection: keep-alive, Close\r\n\r\n" )
    ; ( "close, then a response that does not close"
      , "HTTP/1.1 204 No Content\r\nConnection: close\r\n\r\nHTTP/1.1 204 No Content\r\n\r\n"
      )
    ; ( "huge content-length"
      , "HTTP/1.1 200 OK\r\nContent-Length: 4611686018427387903\r\n\r\nx" )
    ; ( "content-length too large for an int"
      , "HTTP/1.1 200 OK\r\nContent-Length: 99999999999999999999\r\n\r\nx" )
    ; "content-length +2", "HTTP/1.1 200 OK\r\nContent-Length: +2\r\n\r\nxx"
    ; "content-length 0x2", "HTTP/1.1 200 OK\r\nContent-Length: 0x2\r\n\r\nxx"
    ; ( "huge chunk size"
      , "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n3fffffffffffffff\r\nx"
      )
    ; ( "chunk size 1_0"
      , "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n1_0\r\n" )
    ; ( "chunked"
      , "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5;x=y\r\nhello\r\n0\r\nT: 1\r\n\r\n"
      )
    ; ( "chunked, last chunk missing"
      , "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nhello\r\n" )
    ; ( "chunked, bad size"
      , "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\nzz\r\n" )
    ; ( "chunked wins over content-length"
      , "HTTP/1.1 200 OK\r\nContent-Length: 99\r\nTransfer-Encoding: chunked\r\n\r\n0\r\n\r\n"
      )
    ; "delimited by the close", "HTTP/1.1 200 OK\r\n\r\nhello"
    ; "not a response", "garbage\r\n\r\n" ]
