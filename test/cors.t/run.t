The CORS headers that the extension adds. Self-contained POSIX sh.

  $ mkdir -p log data www
  $ echo hello > www/a.txt
  $ cat > conf.xml <<EOF
  > <ocsigen><server>
  >   <port>127.0.0.1:8115</port>
  >   <logdir>log</logdir><datadir>data</datadir>
  >   <extension findlib-package="ocsigenserver.ext.staticmod"/>
  >   <extension findlib-package="ocsigenserver.ext.cors"/>
  >   <host hostfilter="*">
  >     <static dir="www"/>
  >     <cors credentials="true" max_age="60" methods="GET,POST"/>
  >   </host>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c conf.xml >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT
  $ curl -s -o /dev/null --max-time 10 --retry 20 --retry-delay 1 \
  >   --retry-connrefused http://127.0.0.1:8115/a.txt
  $ headers () {
  >   curl -s -D - -o /dev/null "$@" \
  >   | grep -i '^HTTP\|^vary\|^access-control' | tr -d '\r' | sort
  > }

Without an Origin, no CORS header, but the response varies with the Origin:

  $ headers http://127.0.0.1:8115/a.txt
  HTTP/1.1 200 OK
  vary: origin

With an Origin:

  $ headers -H 'Origin: http://a.example' http://127.0.0.1:8115/a.txt
  HTTP/1.1 200 OK
  access-control-allow-credentials: true
  access-control-allow-origin: http://a.example
  access-control-max-age: 60
  vary: origin

A preflight request for an allowed method, then for another one:

  $ headers -X OPTIONS -H 'Origin: http://a.example' \
  >   -H 'Access-Control-Request-Method: POST' http://127.0.0.1:8115/a.txt
  HTTP/1.1 200 OK
  access-control-allow-credentials: true
  access-control-allow-methods: POST
  access-control-allow-origin: http://a.example
  access-control-max-age: 60
  vary: origin
  $ headers -X OPTIONS -H 'Origin: http://a.example' \
  >   -H 'Access-Control-Request-Method: DELETE' http://127.0.0.1:8115/x
  HTTP/1.1 404 Not Found

A refused method on a request that has an answer leaves the answer alone:

  $ headers -H 'Origin: http://a.example' \
  >   -H 'Access-Control-Request-Method: DELETE' http://127.0.0.1:8115/a.txt
  HTTP/1.1 200 OK
  vary: origin

A wrong value of an attribute is a configuration error:

  $ sed 's/credentials="true"/credentials="yes"/; s/8115/8116/' conf.xml \
  >   > bad.xml
  $ ocsigenserver -c bad.xml 2>&1 | grep -o 'The credentials attribute.*'
  The credentials attribute of <cors> is true or false, not "yes"
