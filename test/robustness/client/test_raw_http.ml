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
             (response
            ^ "\r\nHTTP/1.1 404 Not Found\r\nHTTP/1.0 bad\r\nHTTP/1.1 +20 x\r\nHTTP/1.1 0x1F4\r\nHTTP/1.1 2000\r\n"
             ))))
