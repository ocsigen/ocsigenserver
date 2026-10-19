A request body shorter than its Content-Length, because the client closed its
side of the connection, is not handled as if it were complete: reading it
fails, and the request is answered with 400.

  $ mkdir -p log
  $ dune build ./server.exe ./client.exe 2>&1
  $ dune exec -- ./server.exe >server.log 2>&1 &
  $ trap 'echo shutdown > local.cmd 2>/dev/null; wait' EXIT
  $ i=0; while [ ! -e local.cmd ]; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done
  $ i=0; while ! curl -s --max-time 5 http://127.0.0.1:8079/ >/dev/null 2>&1; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done

A raw body, whole, then 10 bytes of the 100 announced:

  $ dune exec -- ./client.exe raw 100
  HTTP/1.1 200 OK: 100
  $ dune exec -- ./client.exe raw 10
  HTTP/1.1 400 Bad Request: Error: Bad Request

A URL-encoded form, whose last value would otherwise be cut short in silence:

  $ dune exec -- ./client.exe form 100
  HTTP/1.1 200 OK: 98
  $ dune exec -- ./client.exe form 10
  HTTP/1.1 400 Bad Request: Error: Bad Request

A multipart form, cut in its parameter, then just before its final boundary:

  $ dune exec -- ./client.exe multipart 100
  HTTP/1.1 200 OK: 42
  $ dune exec -- ./client.exe multipart 60
  HTTP/1.1 400 Bad Request: Error: Bad Request
  $ dune exec -- ./client.exe multipart 90
  HTTP/1.1 400 Bad Request: Error: Bad Request
