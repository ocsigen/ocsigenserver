  $ source ../../server-test-helpers.sh
  $ run_server ./test.exe
  ocsigen:main: [WARNING] Command pipe created
  ocsigen:access:  connection for local-test from unix: (): /index.html
  ocsigen:ext: [INFO] host found! local-test:0 matches .* 
  ocsigen:ext:staticmod: [INFO] Is it a static file?
  ocsigen:local-file: [INFO] Testing "./index.html".
  ocsigen:local-file: [INFO] checking if file index.html can be sent
  ocsigen:ext: [INFO] Compiling exclusion regexp $^
  ocsigen:local-file: [INFO] Returning "./index.html".
  ocsigen:access:  connection for local-test from unix: (): /index.html
  ocsigen:ext: [INFO] host found! local-test:0 matches .* 
  ocsigen:ext:staticmod: [INFO] Is it a static file?
  ocsigen:local-file: [INFO] Testing "./index.html".
  ocsigen:local-file: [INFO] checking if file index.html can be sent
  ocsigen:local-file: [INFO] Returning "./index.html".
  ocsigen:access:  connection for local-test from unix: (): /empty_dir/
  ocsigen:ext: [INFO] host found! local-test:0 matches .* 
  ocsigen:ext:staticmod: [INFO] Is it a static file?
  ocsigen:local-file: [INFO] Testing "./empty_dir/".
  ocsigen:local-file: [INFO] Testing "./empty_dir/index.html" as possible index.
  ocsigen:local-file: [INFO] No index and no listing
  ocsigen:access:  connection for local-test from unix: (): /doesnt_exists.html
  ocsigen:ext: [INFO] host found! local-test:0 matches .* 
  ocsigen:ext:staticmod: [INFO] Is it a static file?
  ocsigen:local-file: [INFO] Testing "./doesnt_exists.html".
  application: [WARNING] Command received: shutdown

Without Accept-Encoding the response is not compressed:

  $ curl_ "index.html"
  HTTP/1.1 200 OK
  content-type: text/html
  server: Ocsigen
  content-length: 12

  Hello world

gzip:

  $ curl_ "index.html" -H "Accept-Encoding: gzip" --compressed
  HTTP/1.1 200 OK
  content-type: text/html
  content-encoding: gzip
  server: Ocsigen
  transfer-encoding: chunked

  Hello world

deflate (zlib format):

  $ curl_ "index.html" -H "Accept-Encoding: deflate" --compressed
  HTTP/1.1 200 OK
  content-type: text/html
  content-encoding: deflate
  server: Ocsigen
  transfer-encoding: chunked

  Hello world

zstd:

  $ curl_ "index.html" -H "Accept-Encoding: zstd" --compressed
  HTTP/1.1 200 OK
  content-type: text/html
  content-encoding: zstd
  server: Ocsigen
  transfer-encoding: chunked

  Hello world

When several codecs are offered, zstd is preferred:

  $ curl_ "index.html" -H "Accept-Encoding: gzip, zstd" --compressed
  HTTP/1.1 200 OK
  content-type: text/html
  content-encoding: zstd
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
