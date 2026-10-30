The securityheaders extension adds security headers to responses.

  $ . ../server-test-helpers.sh
  $ free_ports 2
  $ mkdir www
  $ printf '<html></html>' > www/index.html
  $ cat > site.conf <<EOF
  > <ocsigen><server>
  > <port>127.0.0.1:$PORT1</port>
  > <extension findlib-package="ocsigenserver.ext.staticmod"/>
  > <extension findlib-package="ocsigenserver.ext.securityheaders"/>
  > <host hostfilter="*">
  >   <static dir="www" />
  >   <securityheaders/>
  > </host>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c site.conf >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT
  $ wait_server $SERVER_PID server.log http://127.0.0.1:$PORT1/index.html

The safe-by-default headers are present:

  $ curl -s -D headers -o /dev/null http://127.0.0.1:$PORT1/index.html
  $ grep -i '^x-content-type-options:' headers | tr -d '\r'
  x-content-type-options: nosniff
  $ grep -i '^x-frame-options:' headers | tr -d '\r'
  x-frame-options: SAMEORIGIN
  $ grep -i '^referrer-policy:' headers | tr -d '\r'
  referrer-policy: strict-origin-when-cross-origin

HSTS and CSP are opt-in, so they are absent by default:

  $ grep -i '^strict-transport-security:' headers || echo absent
  absent
  $ grep -i '^content-security-policy:' headers || echo absent
  absent

Opting in adds them, and an attribute can disable a default header:

  $ cat > site2.conf <<EOF
  > <ocsigen><server>
  > <port>127.0.0.1:$PORT2</port>
  > <extension findlib-package="ocsigenserver.ext.staticmod"/>
  > <extension findlib-package="ocsigenserver.ext.securityheaders"/>
  > <host hostfilter="*">
  >   <static dir="www" />
  >   <securityheaders hsts="max-age=31536000"
  >     content-security-policy="default-src 'self'" frame-options="no"/>
  > </host>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c site2.conf >server2.log 2>&1 &
  $ SERVER2_PID=$!
  $ trap 'kill $SERVER_PID $SERVER2_PID 2>/dev/null' EXIT
  $ wait_server $SERVER2_PID server2.log http://127.0.0.1:$PORT2/index.html
  $ curl -s -D h2 -o /dev/null http://127.0.0.1:$PORT2/index.html
  $ grep -i '^strict-transport-security:' h2 | tr -d '\r'
  strict-transport-security: max-age=31536000
  $ grep -i '^content-security-policy:' h2 | tr -d '\r'
  content-security-policy: default-src 'self'
  $ grep -i '^x-frame-options:' h2 || echo absent
  absent
