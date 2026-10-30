Client timeouts: a client cannot hold a connection by sending its request, or
reading its response, too slowly, while a response that takes long to be
produced, or a body that keeps coming fast enough, is not cut. The bounds of the
test server are small (see ../client-timeout/server.ml): a timeout of 2
seconds, a keep-alive timeout of 3 seconds and a minimum data rate of 2 bytes
per second. A second server, the limit server, accepts 4 connections per
address, and a third, the pool server, is full at 4 connections.
../client-timeout/client.ml runs the scenarios, and finds the ports of the
servers in the environment.

  $ . ../server-test-helpers.sh
  $ free_ports 6
  $ export PLAIN_PORT=$PORT1 TLS_PORT=$PORT2 LIMIT_PORT=$PORT3 \
  >   POOL_PORT=$PORT5 POOL_TLS_PORT=$PORT6
  $ mkdir -p log limit/log pool/log
  $ openssl req -x509 -newkey rsa:2048 -nodes -keyout privkey.pem \
  >   -out cert.pem -days 2 -subj /CN=localhost >/dev/null 2>&1
  $ ../client-timeout/server.exe $PLAIN_PORT $TLS_PORT >server.log 2>&1 &
  $ LOCAL=$!
  $ ../client-timeout/server.exe limit $LIMIT_PORT $PORT4 \
  >   >limit/server.log 2>&1 &
  $ LIMIT=$!
  $ ../client-timeout/server.exe pool $POOL_PORT $POOL_TLS_PORT \
  >   >pool/server.log 2>&1 &
  $ POOL=$!
  $ trap 'stop_server $LOCAL; stop_server $LIMIT limit.cmd;
  >   stop_server $POOL pool.cmd' EXIT
  $ wait_server $LOCAL server.log local.cmd http://127.0.0.1:$PLAIN_PORT/
  $ wait_server $LIMIT limit/server.log limit.cmd http://127.0.0.1:$LIMIT_PORT/
  $ wait_server $POOL pool/server.log pool.cmd http://127.0.0.1:$POOL_PORT/

Each scenario waits several seconds for the bounds of the server. They all run
at once, each writing what it saw to a file, shown below, except those of the
pool server, which share it and run one after the other.

  $ scenario () {
  >   ../client-timeout/client.exe "$@" >"$(echo "$@" | tr ' ' -).out" 2>&1; }
  $ (for s in partial-headers drip-headers idle-keepalive keepalive-reuse \
  >     slow-response slow-body stalled-body drip-body slow-reader per-address \
  >     no-handshake "partial-headers tls" "idle-keepalive tls" mute abrupt \
  >     connection-close pipelined-after-close close-among-options \
  >     keep-alive-among-options http-1.0 http-1.0-keep-alive; do
  >    scenario $s &
  >  done
  >  (for s in pool-idle pool-silent pool-handshake pool-busy; do
  >     scenario $s; done) &
  >  wait)

Headers that never end: the connection is closed once the timeout has passed,
without an answer.

  $ cat partial-headers.out
  closed by the server after the timeout, without answering

Headers sent one byte at a time, each well within the timeout: what is bounded
is the time to receive the headers in full, so the connection is closed all the
same. This is the "Slowloris" attack.

  $ cat drip-headers.out
  closed by the server after the timeout, without answering

A connection kept alive after a response, then left idle, is closed after the
keep-alive timeout.

  $ cat idle-keepalive.out
  HTTP/1.1 200 OK: ok
  closed by the server after the keep-alive timeout, without answering

The keep-alive timeout bounds each pause between two requests, not the life of
the connection.

  $ cat keepalive-reuse.out
  HTTP/1.1 200 OK: ok
  HTTP/1.1 200 OK: ok
  HTTP/1.1 200 OK: ok

A response that takes longer than the timeout to be produced is not cut, since
nothing is read from the connection meanwhile. Eliom's comet requests, long
downloads and streamed responses depend on this.

  $ cat slow-response.out
  HTTP/1.1 200 OK: slow response

A body that takes longer than the timeout to arrive, but keeps coming above the
minimum rate, is read in full.

  $ cat slow-body.out
  HTTP/1.1 200 OK: 12

A body that stops coming: the connection is closed without an answer, so the
truncated body is never handled as if it were complete.

  $ cat stalled-body.out
  closed by the server after the timeout, without answering

A body that keeps coming, but below the minimum rate ("R-U-Dead-Yet"), is cut
as well.

  $ cat drip-body.out
  closed by the server before the end of the body, without answering

A client that stops reading a large response does not hold the connection
either: the server cuts it.

  $ cat slow-reader.out
  the server cut the response

The fifth simultaneous connection from the same address is refused.

  $ cat per-address.out
  the fifth connection is refused at once
  once they are closed: HTTP/1.1 200 OK: ok

Over TLS, the handshake is bounded too: a client that connects to the TLS port
and sends nothing is closed after the timeout.

  $ cat no-handshake.out
  closed by the server after the timeout, without answering

Headers that never end over TLS, and an idle kept-alive TLS connection, are
closed the same way as in plain HTTP.

  $ cat partial-headers-tls.out
  closed by the server after the timeout, without answering
  $ cat idle-keepalive-tls.out
  HTTP/1.1 200 OK: ok
  closed by the server after the keep-alive timeout, without answering

The server ends an idle kept-alive TLS connection with a close_notify, and
closes its socket at once, without waiting for the close_notify of the client,
which a client could withhold to hold the connection.

  $ cat mute.out
  HTTP/1.1 200 OK: ok
  close_notify after the keep-alive timeout, then the socket closed at once

The server timeout has no effect, so passing it to Ocsigen.Server.start, as the
test server does, logs a warning.

  $ grep -c "server_timeout has no effect" log/warnings.log
  1

A TLS client that leaves without ending its session, as many do, is not an
error of the server.

  $ cat abrupt.out
  HTTP/1.1 200 OK: ok
  $ grep -c "Unhandled exception" log/errors.log
  0
  [1]

A full server makes room for a new client by closing the connections that their
clients do not use, the oldest first: idle kept-alive connections, connections
on which nothing was sent, and TLS connections whose handshake has not started.
Without this, the fifth connection would wait for one of the others to end.

  $ cat pool-idle.out
  a fifth connection: HTTP/1.1 200 OK: ok
  closed to make room: 1 2
  $ cat pool-silent.out
  a fifth connection: HTTP/1.1 200 OK: ok
  closed to make room: 1 2
  $ cat pool-handshake.out
  a fifth connection: HTTP/1.1 200 OK: ok
  closed to make room: 1 2

Busy connections are not closed: a new client waits until one of them is done,
which then makes room.

  $ cat pool-busy.out
  a fifth connection, once a busy one was done: HTTP/1.1 200 OK: ok
  busy connections answered: 4

A request with "Connection: close", and an HTTP/1.0 request without
"Connection: keep-alive", are answered with "Connection: close", and the
server closes the connection at once (RFC 9112, section 9.6), without serving a
request sent after it. The Connection header is a list of options, whose case
does not matter. An HTTP/1.0 request with "Connection: keep-alive" keeps the
connection open.

  $ cat connection-close.out
  HTTP/1.1 200 OK, Connection: close, responses: 1, closed by the server at once
  $ cat pipelined-after-close.out
  HTTP/1.1 200 OK, Connection: close, responses: 1, closed by the server at once
  $ cat close-among-options.out
  HTTP/1.1 200 OK, Connection: close, responses: 1, closed by the server at once
  $ cat keep-alive-among-options.out
  HTTP/1.1 200 OK, Connection: keep-alive, responses: 1, kept open
  $ cat http-1.0.out
  HTTP/1.1 200 OK, Connection: close, responses: 1, closed by the server at once
  $ cat http-1.0-keep-alive.out
  HTTP/1.1 200 OK, Connection: keep-alive, responses: 1, kept open
