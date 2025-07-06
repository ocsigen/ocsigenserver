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
  application: [WARNING] Command received: shutdown

First response is not compressed:

  $ curl_ "index.html"
  HTTP/1.1 200 OK
  content-type: text/html
  content-length: 12
  server: Ocsigen
  
  Hello world

Second response is compressed:

  $ curl_ "index.html" --compressed
  HTTP/1.1 200 OK
  content-type: text/html
  content-length: 12
  content-encoding: gzip
  server: Ocsigen
  transfer-encoding: chunked
  
  Hello world
