What the reverse proxy sends to its target, and what it sends back. The
upstream server (../revproxy-forwarding/test.ml) answers with what it received,
and the raw one (../revproxy-forwarding/raw.ml) with responses that Ocsigen
Server would not send. The upstream server listens on PORT1 (HTTP) and PORT2
(HTTPS), the raw one on PORT3, and the proxies on the other ports. mask_port
hides PORT1, the port of the target, in the outputs. Self-contained POSIX sh.

  $ . ../server-test-helpers.sh
  $ free_ports 7
  $ mask_port () { sed "s/:$PORT1\$/:UPSTREAM_PORT/"; }
  $ mkdir -p log
  $ openssl req -x509 -newkey rsa:2048 -nodes -keyout privkey.pem \
  >   -out cert.pem -days 2 -subj /CN=localhost \
  >   -addext subjectAltName=DNS:localhost >/dev/null 2>&1
  $ ../revproxy-forwarding/test.exe $PORT1 $PORT2 >upstream.log 2>&1 &
  $ UPSTREAM=$!
  $ ../revproxy-forwarding/raw.exe $PORT3 >raw.log 2>&1 &
  $ RAW=$!
  $ trap 'kill $UPSTREAM $RAW $PROXIES 2>/dev/null; wait' EXIT
  $ wait_server $UPSTREAM upstream.log local.cmd http://127.0.0.1:$PORT1/ \
  >   https://localhost:$PORT2/
  $ wait_server $RAW raw.log http://127.0.0.1:$PORT3/length

A reverse proxy to the HTTP upstream. The path and the query of the request are
forwarded. Uri, which builds the request to the target, encodes the query
again: a "+" becomes "%20", which also means a space in a form, and a ";"
becomes "%3B". X-Forwarded-Proto gives the scheme of the request, and
X-Forwarded-For the address of the client:

  $ ocsigenserver --reverse-proxy http://127.0.0.1:$PORT1 --port $PORT4 \
  >   >proxy.log 2>&1 &
  $ PROXIES="$!"
  $ wait_server $! proxy.log http://127.0.0.1:$PORT4/
  $ curl -s --max-time 10 \
  >   "http://127.0.0.1:$PORT4/a%20b/c?q=1&r=a+b%2Bc&s=x;y" | mask_port
  path: /a%20b/c?q=1&r=a%20b%2Bc&s=x%3By
  host: 127.0.0.1:UPSTREAM_PORT
  https: false
  x-forwarded-proto: http
  x-forwarded-for: 127.0.0.1

A reverse proxy to the HTTPS upstream contacts it over TLS, whatever the scheme
of the request it forwards. The proxy checks the certificate of its target, so
it is told to trust the self-signed one of the upstream:

  $ SSL_CERT_FILE=$PWD/cert.pem \
  >   ocsigenserver --reverse-proxy https://localhost:$PORT2 --port $PORT5 \
  >   >proxy-tls.log 2>&1 &
  $ PROXIES="$PROXIES $!"
  $ wait_server $! proxy-tls.log http://127.0.0.1:$PORT5/
  $ curl -s --max-time 10 http://127.0.0.1:$PORT5/x | grep https
  https: true

A proxy configured in a file, behind a trusted proxy that announces the client
and the scheme of the request it received. The client is the rightmost address
of X-Forwarded-For that is not a trusted proxy, and only this address is sent
to the target:

  $ cat > proxy.conf <<EOF
  > <ocsigen><server>
  > <port>127.0.0.1:$PORT6</port>
  > <logdir>log</logdir>
  > <commandpipe>proxy.cmd</commandpipe>
  > <extension findlib-package="ocsigenserver.ext.accesscontrol"/>
  > <extension findlib-package="ocsigenserver.ext.revproxy"/>
  > <host hostfilter="keephost.example">
  >   <revproxy suburl="(.*)" dest="http://127.0.0.1:$PORT1/\1"
  >     keephost="keephost"/>
  > </host>
  > <host hostfilter="*">
  >   <allow-forward-for proxies="198.51.100.0/24"/>
  >   <allow-forward-proto/>
  >   <revproxy suburl="(.*)" dest="http://127.0.0.1:$PORT1/\1"/>
  > </host>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c proxy.conf >proxy-conf.log 2>&1 &
  $ PROXIES="$PROXIES $!"
  $ wait_server $! proxy-conf.log proxy.cmd http://127.0.0.1:$PORT6/
  $ curl -s --max-time 10 \
  >   -H 'X-Forwarded-For: 10.1.2.3, 192.0.2.1, 198.51.100.1' \
  >   -H 'X-Forwarded-Proto: https' http://127.0.0.1:$PORT6/y \
  >   | mask_port
  path: /y
  host: 127.0.0.1:UPSTREAM_PORT
  https: false
  x-forwarded-proto: https
  x-forwarded-for: 192.0.2.1

With keephost, the proxy sends the Host header of the request to the target,
with the port of the target added since the header has none. It still
connects to the target, not to that host:

  $ curl -s --max-time 10 -H 'Host: keephost.example' \
  >   http://127.0.0.1:$PORT6/z | mask_port
  path: /z
  host: keephost.example:UPSTREAM_PORT
  https: false
  x-forwarded-proto: http
  x-forwarded-for: 127.0.0.1

The headers meant for a single connection are not forwarded (RFC 9110, section
7.6.1): Connection, the headers it names, Keep-Alive, TE and Upgrade.
Connection cannot name the headers that frame the body, nor those the proxy
sets or keeps:

  $ curl -s --max-time 10 -H 'X-Hop: 1' -H 'Keep-Alive: timeout=5' \
  >   -H 'Connection: keep-alive, X-Hop, Content-Length, X-Forwarded-For' \
  >   -H 'TE: trailers' -H 'Upgrade: h2c' -H 'X-End: 1' --data abc \
  >   http://127.0.0.1:$PORT4/headers | mask_port
  accept: */*
  content-length: 3
  content-type: application/x-www-form-urlencoded
  host: 127.0.0.1:UPSTREAM_PORT
  x-end: 1
  x-forwarded-for: 127.0.0.1
  x-forwarded-proto: http
  body: abc
  $ curl -s --max-time 10 -H 'Host: keephost.example' -H 'Connection: Host' \
  >   http://127.0.0.1:$PORT6/z | grep host | mask_port
  host: keephost.example:UPSTREAM_PORT

The response of the target loses its own such headers, and is sent in
HTTP/1.1. A body that ended with the connection to the target ends with the
connection to the client (otherwise, curl would fail with its timeout):

  $ ocsigenserver --reverse-proxy http://127.0.0.1:$PORT3 --port $PORT7 \
  >   >proxy-raw.log 2>&1 &
  $ PROXIES="$PROXIES $!"
  $ wait_server $! proxy-raw.log http://127.0.0.1:$PORT7/length
  $ curl -si --max-time 10 http://127.0.0.1:$PORT7/http-1.0 >http-1.0.out
  $ tr -d '\r' <http-1.0.out | grep -iv '^date:'
  HTTP/1.1 200 OK
  X-End: 1
  connection: close
  server: Ocsigen
  
  body ended by the connection

A target that closes its connection after a body of known length does not
make the proxy close the connection to the client:

  $ curl -si --max-time 10 http://127.0.0.1:$PORT7/length | tr -d '\r' \
  >   | grep -iv '^date:'
  HTTP/1.1 200 OK
  content-length: 3
  server: Ocsigen
  connection: keep-alive
  
  ok

A client that leaves before the end of a response makes the proxy close its
connection to the target, which would otherwise stay open:

  $ curl -s --max-time 10 http://127.0.0.1:$PORT7/endless | head -c 100000 \
  >   >/dev/null
  $ for _ in $(seq 1 50); do
  >   grep -q '/endless' raw.log && break; sleep 0.1
  > done
  $ grep '/endless' raw.log
  /endless: connection closed
