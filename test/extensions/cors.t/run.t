Cors answers preflight requests and adds CORS headers.

  $ . ../../server-test-helpers.sh
  $ printf 'data' > index.html
  $ run_server ./test.exe

A preflight (OPTIONS) request gets the allowed methods and max-age.

  $ curl_ index.html -X OPTIONS -H "Origin: http://example.com" \
  >   -H "Access-Control-Request-Method: GET"
  HTTP/1.1 200 OK
  content-type: text/html
  access-control-max-age: 3600
  access-control-allow-methods: GET
  access-control-allow-origin: http://example.com
  server: Ocsigen
  content-length: 4
  
  data

An actual request is annotated with the allowed origin.

  $ curl_ index.html -H "Origin: http://example.com" \
  >   | grep -i '^access-control-allow-origin:'
  access-control-allow-origin: http://example.com
