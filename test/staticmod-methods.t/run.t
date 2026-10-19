Staticmod serves files and directory listings to GET and HEAD only. OPTIONS
gets the allowed methods, other methods get 405. An error page (code
attribute) answers any method.

  $ . ../server-test-helpers.sh
  $ free_ports 1
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT
  $ mkdir -p log www/dir
  $ echo hello > www/hello.txt && echo x > www/dir/x.txt
  $ echo 'Error page' > error.html
  $ cat > conf.xml <<EOF
  > <ocsigen><server>
  >   <port>127.0.0.1:$PORT1</port>
  >   <logdir>log</logdir>
  >   <extension findlib-package="ocsigenserver.ext.staticmod"/>
  >   <extension findlib-package="ocsigenserver.ext.extendconfiguration"/>
  >   <host hostfilter="*">
  >     <listdirs value="true"/>
  >     <static dir="www"/>
  >     <static code="404" dest="error.html"/>
  >   </host>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c conf.xml >server.log 2>&1 &
  $ SERVER_PID=$!
  $ wait_server $SERVER_PID server.log http://127.0.0.1:$PORT1/hello.txt
  $ req () {
  >   curl -s -i --max-time 10 -X "$1" "http://127.0.0.1:$PORT1/$2" |
  >   tr -d '\r' | grep -viE '^(date|etag|last-modified): '
  > }

A file:

  $ req POST hello.txt
  HTTP/1.1 405 Method Not Allowed
  allow: GET, HEAD, OPTIONS
  content-type: text/plain; charset=utf-8
  server: Ocsigen
  content-length: 25
  connection: keep-alive
  
  Error: Method Not Allowed
  $ req OPTIONS hello.txt
  HTTP/1.1 200 OK
  allow: GET, HEAD, OPTIONS
  server: Ocsigen
  content-length: 0
  connection: keep-alive
  

A directory listing:

  $ req GET dir/ | head -1
  HTTP/1.1 200 OK
  $ req POST dir/
  HTTP/1.1 405 Method Not Allowed
  allow: GET, HEAD, OPTIONS
  content-type: text/plain; charset=utf-8
  server: Ocsigen
  content-length: 25
  connection: keep-alive
  
  Error: Method Not Allowed
  $ req OPTIONS dir/
  HTTP/1.1 200 OK
  allow: GET, HEAD, OPTIONS
  server: Ocsigen
  content-length: 0
  connection: keep-alive
  

The error page of a missing file answers any method, with the status of the
error:

  $ req GET missing.txt
  HTTP/1.1 404 Not Found
  content-type: text/html; charset=utf-8
  accept-ranges: bytes
  server: Ocsigen
  content-length: 11
  connection: keep-alive
  
  Error page
  $ req POST missing.txt
  HTTP/1.1 404 Not Found
  content-type: text/html; charset=utf-8
  accept-ranges: bytes
  server: Ocsigen
  content-length: 11
  connection: keep-alive
  
  Error page
  $ req OPTIONS missing.txt
  HTTP/1.1 404 Not Found
  content-type: text/html; charset=utf-8
  accept-ranges: bytes
  server: Ocsigen
  content-length: 11
  connection: keep-alive
  
  Error page
