  $ . ../../server-test-helpers.sh
  $ run_server ../deflatemod/test.exe
  ocsigen:main: [WARNING] Command pipe created
  ocsigen:ext: [INFO] host found! local-test:0 matches .* 
  ocsigen:ext:staticmod: [INFO] Is it a static file?
  ocsigen:local-file: [INFO] Testing "./index.html".
  ocsigen:local-file: [INFO] checking if file index.html can be sent
  ocsigen:ext: [INFO] Compiling exclusion regexp $^
  ocsigen:local-file: [INFO] Returning "./index.html".
  ocsigen:ext: [INFO] host found! local-test:0 matches .* 
  ocsigen:ext:staticmod: [INFO] Is it a static file?
  ocsigen:local-file: [INFO] Testing "./index.html".
  ocsigen:local-file: [INFO] checking if file index.html can be sent
  ocsigen:local-file: [INFO] Returning "./index.html".
  ocsigen:ext: [INFO] host found! local-test:0 matches .* 
  ocsigen:ext:staticmod: [INFO] Is it a static file?
  ocsigen:local-file: [INFO] Testing "./index.html".
  ocsigen:local-file: [INFO] checking if file index.html can be sent
  ocsigen:local-file: [INFO] Returning "./index.html".
  ocsigen:ext: [INFO] host found! local-test:0 matches .* 
  ocsigen:ext:staticmod: [INFO] Is it a static file?
  ocsigen:local-file: [INFO] Testing "./index.html".
  ocsigen:local-file: [INFO] checking if file index.html can be sent
  ocsigen:local-file: [INFO] Returning "./index.html".
  ocsigen:ext: [INFO] host found! local-test:0 matches .* 
  ocsigen:ext:staticmod: [INFO] Is it a static file?
  ocsigen:local-file: [INFO] Testing "./index.html".
  ocsigen:local-file: [INFO] checking if file index.html can be sent
  ocsigen:local-file: [INFO] Returning "./index.html".
  ocsigen:ext: [INFO] host found! local-test:0 matches .* 
  ocsigen:ext:staticmod: [INFO] Is it a static file?
  ocsigen:local-file: [INFO] Testing "./empty_dir/".
  ocsigen:local-file: [INFO] Testing "./empty_dir/index.html" as possible index.
  ocsigen:local-file: [INFO] No index and no listing
  ocsigen:ext: [INFO] site "missing" does not match url "empty_dir/".
  ocsigen:ext: [INFO] host found! local-test:0 matches .* 
  ocsigen:ext:staticmod: [INFO] Is it a static file?
  ocsigen:local-file: [INFO] Testing "./doesnt_exists.html".
  ocsigen:ext: [INFO] site "missing" does not match url "doesnt_exists.html".
  ocsigen:ext: [INFO] host found! local-test:0 matches .* 
  ocsigen:ext:staticmod: [INFO] Is it a static file?
  ocsigen:local-file: [INFO] Testing "./missing/page.html".
  ocsigen:ext: [INFO] site found: url "missing/page.html" matches "missing".
  ocsigen:ext:staticmod: [INFO] Is it a static file?
  ocsigen:local-file: [INFO] Testing "error.html".
  ocsigen:local-file: [INFO] checking if file error.html can be sent
  ocsigen:local-file: [INFO] Returning "error.html".
  application: [WARNING] Command received: shutdown

Without Accept-Encoding the response is not compressed:

  $ curl_ "index.html"
  HTTP/1.1 200 OK
  content-type: text/html
  accept-ranges: bytes
  server: Ocsigen
  content-length: 12
  connection: keep-alive
  
  Hello world

gzip:

  $ curl_ "index.html" -H "Accept-Encoding: gzip" --compressed
  HTTP/1.1 200 OK
  content-type: text/html
  content-encoding: gzip
  vary: accept-encoding
  server: Ocsigen
  transfer-encoding: chunked
  connection: keep-alive
  
  Hello world


deflate (zlib format):

  $ curl_ "index.html" -H "Accept-Encoding: deflate" --compressed
  HTTP/1.1 200 OK
  content-type: text/html
  content-encoding: deflate
  vary: accept-encoding
  server: Ocsigen
  transfer-encoding: chunked
  connection: keep-alive
  
  Hello world


zstd:

  $ curl_ "index.html" -H "Accept-Encoding: zstd" --compressed
  HTTP/1.1 200 OK
  content-type: text/html
  content-encoding: zstd
  vary: accept-encoding
  server: Ocsigen
  transfer-encoding: chunked
  connection: keep-alive
  
  Hello world


When several codecs are offered, zstd is preferred:

  $ curl_ "index.html" -H "Accept-Encoding: gzip, zstd" --compressed
  HTTP/1.1 200 OK
  content-type: text/html
  content-encoding: zstd
  vary: accept-encoding
  server: Ocsigen
  transfer-encoding: chunked
  connection: keep-alive
  
  Hello world


Querying a directory or a non-existing file should give "Not found" without
compression:

  $ mkdir empty_dir && curl_ empty_dir/ --compressed
  HTTP/1.1 404 Not Found
  server: Ocsigen
  content-length: 16
  connection: keep-alive
  
  Error: Not Found
  $ curl_ doesnt_exists.html --compressed
  HTTP/1.1 404 Not Found
  server: Ocsigen
  content-length: 16
  connection: keep-alive
  
  Error: Not Found

A response with a Content-Range is never compressed, whatever its status: its
range is one of the uncompressed file. Here, a static error page answering a
range request goes out as is, with its length:

  $ curl_ missing/page.html -H "Range: bytes=0-3" -H "Accept-Encoding: gzip"
  HTTP/1.1 404 Not Found
  content-type: text/html
  accept-ranges: bytes
  content-range: bytes 0-3/11
  server: Ocsigen
  content-length: 4
  connection: keep-alive
  
  Erro
