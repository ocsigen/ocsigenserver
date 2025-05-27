  $ source ../../server-test-helpers.sh
  $ run_server ./test.exe
  ocsigen:main: [WARNING] Command pipe created
  ocsigen:access:  connection for local-test from  (curl/8.12.1): /index.html
  ocsigen:ext: [INFO] host found! local-test:0 matches .* 
  ocsigen:ext:staticmod: [INFO] Is it a static file?
  ocsigen:local-file: [INFO] Testing "./index.html".
  ocsigen:local-file: [INFO] checking if file index.html can be sent
  ocsigen:ext: [INFO] Compiling exclusion regexp $^
  ocsigen:local-file: [INFO] Returning "./index.html".
  ocsigen:access:  connection for local-test from  (curl/8.12.1): /index.html
  ocsigen:ext: [INFO] host found! local-test:0 matches .* 
  ocsigen:ext:staticmod: [INFO] Is it a static file?
  ocsigen:local-file: [INFO] Testing "./index.html".
  ocsigen:local-file: [INFO] checking if file index.html can be sent
  ocsigen:local-file: [INFO] Returning "./index.html".
  ocsigen:ext:deflate: [INFO] Zlib stream initialized
  ocsigen:ext:deflate: [INFO] End of stream: big cleaning for zlib
  ocsigen:ext:deflate: [INFO] Zlib.deflate finished, last flush
  ocsigen:ext:deflate: [INFO] Flushing!
  ocsigen:ext:deflate: [INFO] Zlib stream closed
  application: [WARNING] Command received: shutdown

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
