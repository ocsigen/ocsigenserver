The CORS extension.

Cors.run refuses a negative max_age, as the configuration file does:

  $ ../cors/test.exe
  Cors.run ~max_age:(-1) () -> Invalid_argument: Cors.run: negative max_age -1
  Cors.run ~max_age:0 () -> accepted
  Cors.run () -> accepted

The headers that the extension adds:

  $ . ../server-test-helpers.sh
  $ free_ports 1
  $ mkdir -p log data www
  $ echo hello > www/a.txt
  $ cat > conf.xml <<EOF
  > <ocsigen><server>
  >   <port>127.0.0.1:$PORT1</port>
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
  $ wait_server $SERVER_PID server.log http://127.0.0.1:$PORT1/a.txt
  $ headers () {
  >   path=$1; shift
  >   curl -s -D - -o /dev/null "$@" "http://127.0.0.1:$PORT1/$path" \
  >   | grep -i '^HTTP\|^vary\|^access-control' | tr -d '\r' | sort
  > }

Without an Origin, no CORS header:

  $ headers a.txt
  HTTP/1.1 200 OK

With an Origin:

  $ headers a.txt -H 'Origin: http://a.example'
  HTTP/1.1 200 OK
  access-control-allow-credentials: true
  access-control-allow-origin: http://a.example
  access-control-max-age: 60

A preflight request for an allowed method, then for another one:

  $ headers x -X OPTIONS -H 'Origin: http://a.example' \
  >   -H 'Access-Control-Request-Method: POST'
  HTTP/1.1 200 OK
  access-control-allow-credentials: true
  access-control-allow-methods: POST
  access-control-allow-origin: http://a.example
  access-control-max-age: 60
  $ headers x -X OPTIONS -H 'Origin: http://a.example' \
  >   -H 'Access-Control-Request-Method: DELETE'
  HTTP/1.1 404 Not Found

An OPTIONS request that another extension answers is left as it is when it
asks for a refused method:

  $ headers a.txt -X OPTIONS -H 'Origin: http://a.example' \
  >   -H 'Access-Control-Request-Method: DELETE'
  HTTP/1.1 200 OK
