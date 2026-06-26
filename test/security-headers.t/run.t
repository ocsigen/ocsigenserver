The securityheaders extension adds security headers to responses.

  $ mkdir www
  $ printf '<html></html>' > www/index.html
  $ cat > site.conf <<'EOF'
  > <ocsigen><server>
  > <port>8067</port>
  > <extension findlib-package="ocsigenserver.ext.staticmod"/>
  > <extension findlib-package="ocsigenserver.ext.securityheaders"/>
  > <host hostfilter="*">
  >   <static dir="www" />
  >   <securityheaders content-security-policy="default-src 'self'"/>
  > </host>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c site.conf >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT

The default headers and the configured Content-Security-Policy are present:

  $ curl -s -D headers -o /dev/null --retry 20 --retry-delay 1 \
  >   --retry-connrefused http://127.0.0.1:8067/index.html
  $ grep -i '^x-content-type-options:' headers | tr -d '\r'
  x-content-type-options: nosniff
  $ grep -i '^x-frame-options:' headers | tr -d '\r'
  x-frame-options: SAMEORIGIN
  $ grep -i '^strict-transport-security:' headers | tr -d '\r'
  strict-transport-security: max-age=15552000; includeSubDomains
  $ grep -i '^content-security-policy:' headers | tr -d '\r'
  content-security-policy: default-src 'self'
