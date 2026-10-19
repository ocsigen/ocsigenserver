Request line and header fields (RFC 9112 sections 2 to 5, RFC 9110 section
5.5).

  $ . ./fixture/helpers.sh
  $ start_server 8475
  $ send () { rawhttp send --port 8475 --idle 0.5 --status; }

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
