What the reverse proxy sends to its target, and what it sends back. The
upstream server (test.ml) answers with what it received, and the raw one
(raw.ml) with responses that Ocsigen Server would not send. Self-contained
POSIX sh.

  $ . ../server-test-helpers.sh
  $ mkdir -p log
  $ openssl req -x509 -newkey rsa:2048 -nodes -keyout privkey.pem \
  >   -out cert.pem -days 2 -subj /CN=localhost \
  >   -addext subjectAltName=DNS:localhost >/dev/null 2>&1
  $ dune build ./test.exe ./raw.exe 2>&1
  $ ./_build/default/test.exe >upstream.log 2>&1 &
  $ UPSTREAM="$!"
  $ ./_build/default/raw.exe >raw.log 2>&1 &
  $ UPSTREAM="$UPSTREAM $!"
  $ trap 'kill $UPSTREAM $PROXIES 2>/dev/null; wait' EXIT
  $ wait_http http://127.0.0.1:8073/
  $ wait_http -k https://localhost:8064/
  $ wait_http http://127.0.0.1:8069/length

A reverse proxy to the HTTP upstream. The path and the query of the request are
forwarded. Uri, which builds the request to the target, encodes the query
again: a "+" becomes "%20", which also means a space in a form, and a ";"
becomes "%3B". X-Forwarded-Proto gives the scheme of the request, and
X-Forwarded-For the address of the client:

  $ ocsigenserver --reverse-proxy http://127.0.0.1:8073 --port 8074 \
  >   >proxy.log 2>&1 &
  $ PROXIES="$!"
  $ wait_http http://127.0.0.1:8074/
  $ curl -s --max-time 10 'http://127.0.0.1:8074/a%20b/c?q=1&r=a+b%2Bc&s=x;y'
  path: /a%20b/c?q=1&r=a%20b%2Bc&s=x%3By
  host: 127.0.0.1:8073
  https: false
  x-forwarded-proto: http
  x-forwarded-for: 127.0.0.1

A reverse proxy to the HTTPS upstream contacts it over TLS, whatever the scheme
of the request it forwards. The proxy checks the certificate of its target, so
it is told to trust the self-signed one of the upstream:

  $ SSL_CERT_FILE=$PWD/cert.pem \
  >   ocsigenserver --reverse-proxy https://localhost:8064 --port 8075 \
  >   >proxy-tls.log 2>&1 &
  $ PROXIES="$PROXIES $!"
  $ wait_http http://127.0.0.1:8075/
  $ curl -s --max-time 10 http://127.0.0.1:8075/x | grep https
  https: true

A proxy configured in a file, behind a trusted proxy that announces the client
and the scheme of the request it received:

  $ cat > proxy.conf <<'EOF'
  > <ocsigen><server>
  > <port>8081</port>
  > <logdir>log</logdir>
  > <commandpipe>proxy.cmd</commandpipe>
  > <extension findlib-package="ocsigenserver.ext.accesscontrol"/>
  > <extension findlib-package="ocsigenserver.ext.revproxy"/>
  > <host hostfilter="keephost.example">
  >   <revproxy suburl="(.*)" dest="http://127.0.0.1:8073/\1" keephost="keephost"/>
  > </host>
  > <host hostfilter="*">
  >   <allow-forward-for/>
  >   <allow-forward-proto/>
  >   <revproxy suburl="(.*)" dest="http://127.0.0.1:8073/\1"/>
  > </host>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c proxy.conf >proxy-conf.log 2>&1 &
  $ PROXIES="$PROXIES $!"
  $ wait_http http://127.0.0.1:8081/
  $ curl -s --max-time 10 -H 'X-Forwarded-For: 192.0.2.1, 198.51.100.1' \
  >   -H 'X-Forwarded-Proto: https' http://127.0.0.1:8081/y
  path: /y
  host: 127.0.0.1:8073
  https: false
  x-forwarded-proto: https
  x-forwarded-for: 192.0.2.1, 198.51.100.1

With keephost, the proxy sends the Host header of the request to the target,
with the port of the target added since the header has none. It still
connects to the target, not to that host:

  $ curl -s --max-time 10 -H 'Host: keephost.example' http://127.0.0.1:8081/z
  path: /z
  host: keephost.example:8073
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
  >   http://127.0.0.1:8074/headers
  accept: */*
  content-length: 3
  content-type: application/x-www-form-urlencoded
  host: 127.0.0.1:8073
  x-end: 1
  x-forwarded-for: 127.0.0.1
  x-forwarded-proto: http
  body: abc
  $ curl -s --max-time 10 -H 'Host: keephost.example' -H 'Connection: Host' \
  >   http://127.0.0.1:8081/z | grep host
  host: keephost.example:8073

The response of the target loses its own such headers, and is sent in
HTTP/1.1. A body that ended with the connection to the target ends with the
connection to the client (otherwise, curl would fail with its timeout):

  $ ocsigenserver --reverse-proxy http://127.0.0.1:8069 --port 8070 \
  >   >proxy-raw.log 2>&1 &
  $ PROXIES="$PROXIES $!"
  $ wait_http http://127.0.0.1:8070/length
  $ curl -si --max-time 10 http://127.0.0.1:8070/http-1.0 >http-1.0.out
  $ tr -d '\r' <http-1.0.out | grep -iv '^date:'
  HTTP/1.1 200 OK
  X-End: 1
  connection: close
  server: Ocsigen
  
  body ended by the connection

A target that closes its connection after a body of known length does not
make the proxy close the connection to the client:

  $ curl -si --max-time 10 http://127.0.0.1:8070/length | tr -d '\r' \
  >   | grep -iv '^date:'
  HTTP/1.1 200 OK
  content-length: 3
  server: Ocsigen
  connection: keep-alive
  
  ok
