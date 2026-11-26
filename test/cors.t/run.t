The CORS extension.

Cors.run refuses a negative max_age, as the configuration file does:

  $ ../cors/test.exe
  Cors.run ~max_age:(-1) () -> Invalid_argument: Cors.run: negative max_age -1
  Cors.run ~max_age:0 () -> accepted
  Cors.run () -> accepted

The headers that the extension adds. Under restricted/, only the origin
http://a.example is allowed, and the other requests get Vary: Origin from
<outputfilter>, as the manual recommends:

  $ . ../server-test-helpers.sh
  $ free_ports 1
  $ mkdir -p log www/restricted
  $ echo hello > www/a.txt
  $ echo hello > www/restricted/b.txt
  $ cat > conf.xml <<EOF
  > <ocsigen><server>
  >   <port>127.0.0.1:$PORT1</port>
  >   <logdir>log</logdir>
  >   <extension findlib-package="ocsigenserver.ext.staticmod"/>
  >   <extension findlib-package="ocsigenserver.ext.accesscontrol"/>
  >   <extension findlib-package="ocsigenserver.ext.outputfilter"/>
  >   <extension findlib-package="ocsigenserver.ext.cors"/>
  >   <extension findlib-package="ocsigenserver.ext.deflatemod"/>
  >   <host hostfilter="*">
  >     <static dir="www"/>
  >     <deflate compress="only"><type>text/plain</type></deflate>
  >     <if><path regexp="restricted/.*"/>
  >       <then>
  >         <if><header name="origin" regexp="http://a\.example"/>
  >           <then><cors/></then>
  >           <else>
  >             <outputfilter header="Vary" dest="Origin" replace="false"/>
  >           </else>
  >         </if>
  >       </then>
  >       <else><cors credentials="true" max_age="60" methods="GET,POST"/></else>
  >     </if>
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

Without an Origin, no CORS header, but the response varies with the Origin.
It also varies with Accept-Encoding, as <deflate> compresses text/plain:

  $ headers a.txt
  HTTP/1.1 200 OK
  vary: accept-encoding, origin

With an Origin:

  $ headers a.txt -H 'Origin: http://a.example'
  HTTP/1.1 200 OK
  access-control-allow-credentials: true
  access-control-allow-origin: http://a.example
  access-control-max-age: 60
  vary: accept-encoding, origin

When <deflate> has already compressed the response, <cors/> adds Origin to
its Vary line:

  $ headers a.txt -H 'Origin: http://a.example' -H 'Accept-Encoding: gzip'
  HTTP/1.1 200 OK
  access-control-allow-credentials: true
  access-control-allow-origin: http://a.example
  access-control-max-age: 60
  vary: accept-encoding, origin

A preflight request for an allowed method, then for another one:

  $ headers x -X OPTIONS -H 'Origin: http://a.example' \
  >   -H 'Access-Control-Request-Method: POST'
  HTTP/1.1 200 OK
  access-control-allow-credentials: true
  access-control-allow-methods: POST
  access-control-allow-origin: http://a.example
  access-control-max-age: 60
  vary: origin
  $ headers x -X OPTIONS -H 'Origin: http://a.example' \
  >   -H 'Access-Control-Request-Method: DELETE'
  HTTP/1.1 404 Not Found

An OPTIONS request that another extension answers gets only Vary: Origin
when it asks for a refused method:

  $ headers a.txt -X OPTIONS -H 'Origin: http://a.example' \
  >   -H 'Access-Control-Request-Method: DELETE'
  HTTP/1.1 200 OK
  vary: origin

Under restricted/, the allowed origin, another one, and no origin.
<outputfilter> adds its own Vary line:

  $ headers restricted/b.txt -H 'Origin: http://a.example'
  HTTP/1.1 200 OK
  access-control-allow-origin: http://a.example
  vary: accept-encoding, origin
  $ headers restricted/b.txt -H 'Origin: http://b.example'
  HTTP/1.1 200 OK
  vary: Origin
  vary: accept-encoding
  $ headers restricted/b.txt
  HTTP/1.1 200 OK
  vary: Origin
  vary: accept-encoding
