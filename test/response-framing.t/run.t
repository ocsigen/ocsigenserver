The headers of a response announce the framing of its body, whatever other
headers say about its length.

  $ . ../server-test-helpers.sh
  $ mkdir -p log
  $ ../response-framing/test.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'stop_server $SERVER_PID' EXIT
  $ wait_server $SERVER_PID server.log local.sock local.cmd
  $ get () {
  >   curl --unix-socket local.sock -s -i --raw --max-time 10 "$@" \
  >   | grep -viE '^date: ' | tr -d '\r'
  > }

A chunked body with a Content-Range is sent chunked, not as the fixed length
that the Content-Range alone suggests:

  $ get http://x/range-chunked
  HTTP/1.1 206 Partial Content
  content-range: bytes 0-4/10
  server: Ocsigen
  transfer-encoding: chunked
  connection: keep-alive
  
  5
  01234
  0
  

A Content-Length set by the producer is dropped when the body is chunked:

  $ get http://x/length-chunked
  HTTP/1.1 200 OK
  server: Ocsigen
  transfer-encoding: chunked
  connection: keep-alive
  
  5
  hello
  0
  

A fixed-length body with a Content-Range gets its Content-Length:

  $ get http://x/range-fixed
  HTTP/1.1 206 Partial Content
  content-range: bytes 5-9/10
  server: Ocsigen
  content-length: 5
  connection: keep-alive
  
  56789

A Content-Length set by the producer is replaced by the length of a
fixed-length body:

  $ get http://x/length-fixed
  HTTP/1.1 200 OK
  content-length: 5
  server: Ocsigen
  connection: keep-alive
  
  world

The connection stays usable after each of them:

  $ curl --unix-socket local.sock -s --max-time 10 http://x/range-chunked \
  >   http://x/length-chunked http://x/range-fixed http://x/length-fixed; echo
  01234hello56789world

A response to HEAD has the headers of the response to GET, framing included,
but no body: on a connection, the next response follows its headers.

  $ ../response-framing/client.exe 'HEAD range-chunked' 'HEAD length-fixed' \
  >   'GET range-fixed' | grep -viE '^date: ' | tr -d '\r'
  HTTP/1.1 206 Partial Content
  content-range: bytes 0-4/10
  server: Ocsigen
  transfer-encoding: chunked
  connection: keep-alive
  
  HTTP/1.1 200 OK
  content-length: 5
  server: Ocsigen
  connection: keep-alive
  
  HTTP/1.1 206 Partial Content
  content-range: bytes 5-9/10
  server: Ocsigen
  content-length: 5
  connection: close
  
  56789

A response with a 204 or 304 status has no body, nor any framing header:

  $ get http://x/no-content
  HTTP/1.1 204 No Content
  server: Ocsigen
  connection: keep-alive
  
  $ get http://x/not-modified
  HTTP/1.1 304 Not Modified
  server: Ocsigen
  connection: keep-alive
  
