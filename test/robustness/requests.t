Request line and header fields (RFC 9112 sections 2 to 5, RFC 9110 section
5.5).

  $ . ./fixture/helpers.sh
  $ start_server 8475
  $ send () { rawhttp send --port 8475 --idle 0.5 --status; }

Empty lines before a request are ignored (RFC 9112 section 2.2), as after a
POST body followed by an extra line end: the request is a GET, as the access
log shows.

  $ printf '\r\n\r\nGET /hello.txt HTTP/1.1\r\nHost: x\r\n\r\n' | send
  HTTP/1.1 200 OK
  [idle]
  $ printf 'POST /index.html HTTP/1.1\r\nHost: x\r\nContent-Length: 2\r\n\r\nab\r\nGET /hello.txt HTTP/1.1\r\nHost: x\r\n\r\n' |
  >   rawhttp send --port 8475 --idle 0.5 | grep -o 'HTTP/1\.1 [0-9]*'
  HTTP/1.1 405
  HTTP/1.1 200
  $ grep -c '"GET /hello.txt HTTP/1.1" 200' log/access.log
  2

A method that is not a token is rejected.

  $ printf 'G(ET /hello.txt HTTP/1.1\r\nHost: x\r\n\r\n' | send
  HTTP/1.1 400 Bad Request
  [closed]
  $ printf 'G\001ET /hello.txt HTTP/1.1\r\nHost: x\r\n\r\n' | send
  HTTP/1.1 400 Bad Request
  [closed]

A static file is only answered to GET and HEAD. OPTIONS gets the allowed
methods without content, as for the default health check of HAProxy (RFC
9110 section 9.3.7). Other methods get 405, with the allowed methods
(section 15.5.6). Methods are case-sensitive: "get" is not GET.

  $ printf 'OPTIONS / HTTP/1.0\r\nHost: x\r\n\r\n' |
  >   rawhttp send --port 8475 --idle 0.5
  HTTP/1.1 200 OK
  allow: GET, HEAD, OPTIONS
  x-content-type-options: nosniff
  x-frame-options: SAMEORIGIN
  referrer-policy: strict-origin-when-cross-origin
  server: Ocsigen
  content-length: 0
  connection: close
  
  [closed]

  $ printf 'POST /hello.txt HTTP/1.1\r\nHost: x\r\nContent-Length: 0\r\n\r\n' |
  >   rawhttp send --port 8475 --idle 0.5
  HTTP/1.1 405 Method Not Allowed
  allow: GET, HEAD, OPTIONS
  content-type: text/plain; charset=utf-8
  x-content-type-options: nosniff
  x-frame-options: SAMEORIGIN
  referrer-policy: strict-origin-when-cross-origin
  server: Ocsigen
  content-length: 18
  connection: keep-alive
  
  Method Not Allowed
  [idle]
  $ printf 'get /hello.txt HTTP/1.1\r\nHost: x\r\n\r\n' | send
  HTTP/1.1 405 Method Not Allowed
  [idle]
  $ printf 'FROB /hello.txt HTTP/1.1\r\nHost: x\r\n\r\n' | send
  HTTP/1.1 405 Method Not Allowed
  [idle]
  $ printf 'TRACE / HTTP/1.1\r\nHost: x\r\n\r\n' | send
  HTTP/1.1 405 Method Not Allowed
  [idle]

The server is not a forward proxy: CONNECT is answered 501, without a tunnel.

  $ printf 'CONNECT 127.0.0.1:22 HTTP/1.1\r\nHost: 127.0.0.1:22\r\n\r\n' | send
  HTTP/1.1 501 Not Implemented
  [closed]

The "*" target is for OPTIONS only (RFC 9112 section 3.2.4). OPTIONS * is
about the server as a whole, and is answered without content.

  $ printf 'GET * HTTP/1.1\r\nHost: x\r\n\r\n' | send
  HTTP/1.1 400 Bad Request
  [closed]
  $ printf 'OPTIONS * HTTP/1.1\r\nHost: x\r\n\r\n' |
  >   rawhttp send --port 8475 --idle 0.5
  HTTP/1.1 200 OK
  server: Ocsigen
  content-length: 0
  connection: keep-alive
  
  [idle]

A NUL byte, or another control character, in a field value is rejected
(RFC 9110 section 5.5), and the connection closed. A tab and non-ASCII bytes
are allowed.

  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\nX-A: a\000b\r\n\r\n' | send
  HTTP/1.1 400 Bad Request
  [closed]
  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\nX-A: a\033b\r\n\r\n' | send
  HTTP/1.1 400 Bad Request
  [closed]
  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\nX-A: a\177b\r\n\r\n' | send
  HTTP/1.1 400 Bad Request
  [closed]
  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\nX-A: a\tb\351\r\n\r\n' | send
  HTTP/1.1 200 OK
  [idle]
