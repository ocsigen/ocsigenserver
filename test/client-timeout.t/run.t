Client timeout: the reads on a connection are bounded, so that a client sending
its request too slowly cannot hold a connection, while a response that takes
long to be produced, or a body that keeps coming, is not cut. The timeout of the
test server is one second (see server.ml), and client.ml runs the scenarios.

  $ mkdir -p log data
  $ openssl req -x509 -newkey rsa:2048 -nodes -keyout privkey.pem \
  >   -out cert.pem -days 2 -subj /CN=localhost >/dev/null 2>&1
  $ dune build ./server.exe ./client.exe 2>&1
  $ dune exec -- ./server.exe >server.log 2>&1 &
  $ trap 'echo shutdown > local.cmd 2>/dev/null; wait' EXIT
  $ i=0; while [ ! -e local.cmd ]; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done
  $ i=0; while ! curl -s --max-time 5 http://127.0.0.1:8076/ >/dev/null 2>&1; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done

Headers that never end: the connection is closed once the timeout has passed,
without an answer.

  $ dune exec -- ./client.exe partial-headers
  closed by the server after the timeout, without answering

Headers sent one byte at a time, each well within the timeout: what is bounded
is the time to receive the headers in full, so the connection is closed all the
same. This is the "Slowloris" attack.

  $ dune exec -- ./client.exe drip-headers
  closed by the server after the timeout, without answering

A connection kept alive after a response, then left idle, is closed too.

  $ dune exec -- ./client.exe idle-keepalive
  HTTP/1.1 200 OK: ok
  closed by the server after the timeout, without answering

A response that takes longer than the timeout to be produced is not cut, since
nothing is read from the connection meanwhile. Eliom's comet requests, long
downloads and streamed responses depend on this.

  $ dune exec -- ./client.exe slow-response
  HTTP/1.1 200 OK: slow response

A body that takes longer than the timeout to arrive, but keeps coming, is read
in full.

  $ dune exec -- ./client.exe slow-body
  HTTP/1.1 200 OK: 12

A body that stops coming: the connection is closed without an answer, so the
truncated body is never handled as if it were complete.

  $ dune exec -- ./client.exe stalled-body
  closed by the server after the timeout, without answering

The same holds over TLS, once the handshake is done.

  $ dune exec -- ./client.exe partial-headers tls
  closed by the server after the timeout, without answering

The server timeout has no effect, so passing it to Ocsigen.Server.start, as the
test server does, logs a warning.

  $ grep -c "server_timeout has no effect" log/warnings.log
  1
