A request body shorter than its Content-Length, because the client closed its
side of the connection, is not handled as if it were complete: reading it
fails, and the request is answered with 400.

  $ . ../server-test-helpers.sh
  $ mkdir -p log
  $ ../truncated-body/server.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'stop_server $SERVER_PID' EXIT
  $ wait_server $SERVER_PID server.log local.sock local.cmd

A raw body, whole, then 10 bytes of the 100 announced:

  $ ../truncated-body/client.exe raw 100
  HTTP/1.1 200 OK: 100
  $ ../truncated-body/client.exe raw 10
  HTTP/1.1 400 Bad Request: Error: Bad Request

A URL-encoded form, whose last value would otherwise be cut short in silence:

  $ ../truncated-body/client.exe form 100
  HTTP/1.1 200 OK: 98
  $ ../truncated-body/client.exe form 10
  HTTP/1.1 400 Bad Request: Error: Bad Request

A multipart form, cut in its parameter, then just before its final boundary:

  $ ../truncated-body/client.exe multipart 100
  HTTP/1.1 200 OK: 42
  $ ../truncated-body/client.exe multipart 60
  HTTP/1.1 400 Bad Request: Error: Bad Request
  $ ../truncated-body/client.exe multipart 90
  HTTP/1.1 400 Bad Request: Error: Bad Request
