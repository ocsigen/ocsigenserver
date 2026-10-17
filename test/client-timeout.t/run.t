Client timeouts: a client cannot hold a connection by sending its request, or
reading its response, too slowly, while a response that takes long to be
produced, or a body that keeps coming fast enough, is not cut. The bounds of the
test server are small (see server.ml): a timeout of 2 seconds, a keep-alive
timeout of 3 seconds and a minimum data rate of 2 bytes per second. A second
server, the limit server, accepts 4 connections per address. client.ml runs
the scenarios.

  $ mkdir -p log data limit/log limit/data
  $ openssl req -x509 -newkey rsa:2048 -nodes -keyout privkey.pem \
  >   -out cert.pem -days 2 -subj /CN=localhost >/dev/null 2>&1
  $ dune build ./server.exe ./client.exe 2>&1
  $ ./_build/default/server.exe >server.log 2>&1 &
  $ ./_build/default/server.exe limit >limit/server.log 2>&1 &
  $ trap 'for p in local limit; do echo shutdown > $p.cmd 2>/dev/null; done; wait' EXIT
  $ i=0; while [ ! -e local.cmd ] || [ ! -e limit.cmd ]; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done
  $ for port in 8076 8077; do
  >   i=0; while ! curl -s --max-time 5 http://127.0.0.1:$port/ >/dev/null 2>&1; do
  >     i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done; done

Each scenario waits several seconds for the bounds of the server. They all run
at once, each writing what it saw to a file, shown below.

  $ scenario () {
  >   ./_build/default/client.exe "$@" >"$(echo "$@" | tr ' ' -).out" 2>&1; }
  $ (for s in partial-headers drip-headers idle-keepalive keepalive-reuse \
  >     slow-response slow-body stalled-body drip-body slow-reader per-address \
  >     no-handshake "partial-headers tls" "idle-keepalive tls" mute abrupt; do
  >    scenario $s &
  >  done; wait)

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
