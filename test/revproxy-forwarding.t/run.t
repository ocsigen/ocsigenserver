What the reverse proxy sends to its target. The upstream server answers with
what it received. Self-contained POSIX sh.

  $ mkdir -p log data
  $ openssl req -x509 -newkey rsa:2048 -nodes -keyout privkey.pem \
  >   -out cert.pem -days 2 -subj /CN=localhost \
  >   -addext subjectAltName=DNS:localhost >/dev/null 2>&1
  $ dune build ./test.exe 2>&1
  $ ./_build/default/test.exe >upstream.log 2>&1 &
  $ UPSTREAM="$!"
  $ trap 'kill $UPSTREAM $PROXIES 2>/dev/null; wait' EXIT
  $ curl -s -o /dev/null --max-time 10 --retry 20 --retry-delay 1 \
  >   --retry-connrefused http://127.0.0.1:8073/

A reverse proxy to the HTTP upstream. The path and the query of the request are
forwarded. Uri, which builds the request to the target, encodes the query
again: a "+" becomes "%20", which also means a space in a form, and a ";"
becomes "%3B". X-Forwarded-Proto gives the scheme of the request, and
X-Forwarded-For the address of the client:

  $ ocsigenserver --reverse-proxy http://127.0.0.1:8073 --port 8074 \
  >   >proxy.log 2>&1 &
  $ PROXIES="$!"
  $ curl -s --max-time 10 --retry 20 --retry-delay 1 --retry-connrefused \
  >   'http://127.0.0.1:8074/a%20b/c?q=1&r=a+b%2Bc&s=x;y'
  path: /a%20b/c?q=1&r=a%20b%2Bc&s=x%3By
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
  $ curl -s --max-time 10 --retry 20 --retry-delay 1 --retry-connrefused \
  >   http://127.0.0.1:8075/x | grep https
  https: true

A proxy configured in a file, behind a trusted proxy that announces the client
and the scheme of the request it received:

  $ cat > proxy.conf <<'EOF'
  > <ocsigen><server>
  > <port>8081</port>
  > <logdir>log</logdir><datadir>data</datadir>
  > <commandpipe>proxy.cmd</commandpipe>
  > <extension findlib-package="ocsigenserver.ext.accesscontrol"/>
  > <extension findlib-package="ocsigenserver.ext.revproxy"/>
  > <host hostfilter="*">
  >   <allow-forward-for/>
  >   <allow-forward-proto/>
  >   <revproxy suburl="(.*)" dest="http://127.0.0.1:8073/\1"/>
  > </host>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c proxy.conf >proxy-conf.log 2>&1 &
  $ PROXIES="$PROXIES $!"
  $ curl -s --max-time 10 --retry 20 --retry-delay 1 --retry-connrefused \
  >   -H 'X-Forwarded-For: 192.0.2.1, 198.51.100.1' \
  >   -H 'X-Forwarded-Proto: https' http://127.0.0.1:8081/y
  path: /y
  https: false
  x-forwarded-proto: https
  x-forwarded-for: 192.0.2.1, 198.51.100.1
