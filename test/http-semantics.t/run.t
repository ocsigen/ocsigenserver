Core HTTP behaviour of a static server: methods, status codes and the basic
entity headers (content-type, content-length).

  $ . ../server-test-helpers.sh
  $ printf '<html>hello</html>' > index.html
  $ printf 'plain text\n' > notes.txt
  $ printf 'body{}' > style.css
  $ run_server ../http_semantics_server.exe

A GET on an existing file returns 200 with the right content type and length.

  $ curl_ index.html
  HTTP/1.1 200 OK
  content-type: text/html
  server: Ocsigen
  content-length: 18
  
  <html>hello</html>

A HEAD returns the same headers but no body.

  $ curl_ index.html --head
  HTTP/1.1 200 OK
  content-type: text/html
  server: Ocsigen
  content-length: 18
  

The content type is derived from the file extension.

  $ curl_ notes.txt --head
  HTTP/1.1 200 OK
  content-type: text/plain
  server: Ocsigen
  content-length: 11
  
  $ curl_ style.css --head
  HTTP/1.1 200 OK
  content-type: text/css
  server: Ocsigen
  content-length: 6
  

A missing file gives a 404 with a body.

  $ curl_ nope.html
  HTTP/1.1 404 Not Found
  server: Ocsigen
  content-length: 16
  
  Error: Not Found

The Content-Length header matches the body size.

  $ curl_ notes.txt
  HTTP/1.1 200 OK
  content-type: text/plain
  server: Ocsigen
  content-length: 11
  
  plain text
