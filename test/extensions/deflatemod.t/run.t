  $ . ../../server-test-helpers.sh
  $ run_server ../deflatemod_server.exe

First response is not compressed:

  $ curl_ "index.html"
  HTTP/1.1 200 OK
  content-type: text/html
  server: Ocsigen
  content-length: 12
  
  Hello world

Second response is compressed:

  $ curl_ "index.html" --compressed
  HTTP/1.1 200 OK
  content-type: text/html
  content-encoding: gzip
  server: Ocsigen
  transfer-encoding: chunked
  
  Hello world

Querying a directory or a non-existing file should give "Not found" without
compression:

  $ mkdir empty_dir && curl_ empty_dir/ --compressed
  HTTP/1.1 404 Not Found
  server: Ocsigen
  content-length: 16
  
  Error: Not Found
  $ curl_ doesnt_exists.html --compressed
  HTTP/1.1 404 Not Found
  server: Ocsigen
  content-length: 16
  
  Error: Not Found
