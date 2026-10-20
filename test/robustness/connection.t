Connection management and the Host header (RFC 9112 sections 3.2 and 9).

  $ . ./fixture/helpers.sh
  $ make_certificate
  $ start_server 8471 8481
  $ send () { rawhttp send --port 8471 --idle 0.5 --status; }

With "Connection: close", the server closes the connection after the
response, which says "Connection: close" too (RFC 9112 section 9.6). A
request that follows on the same connection is not answered. Connection
options are case-insensitive, and the field may be a list.

  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n' |
  >   rawhttp send --port 8471 --idle 0.5
  HTTP/1.1 200 OK
  content-type: text/plain
  accept-ranges: bytes
  x-content-type-options: nosniff
  x-frame-options: SAMEORIGIN
  referrer-policy: strict-origin-when-cross-origin
  server: Ocsigen
  content-length: 6
  connection: close
  
  hello
  [closed]
  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\nGET /index.html HTTP/1.1\r\nHost: x\r\n\r\n' |
  >   send
  HTTP/1.1 200 OK
  [closed]
  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\nConnection: Close\r\n\r\n' |
  >   send
  HTTP/1.1 200 OK
  [closed]
  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\nConnection: keep-alive, close\r\n\r\n' |
  >   send
  HTTP/1.1 200 OK
  [closed]
  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\nConnection: TE\r\n\r\n' |
  >   send
  HTTP/1.1 200 OK
  [idle]

The whole response is sent before the connection is closed, over TLS too.

  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n' |
  >   rawhttp send --port 8481 --tls --idle 0.5
  HTTP/1.1 200 OK
  content-type: text/plain
  accept-ranges: bytes
  x-content-type-options: nosniff
  x-frame-options: SAMEORIGIN
  referrer-policy: strict-origin-when-cross-origin
  server: Ocsigen
  content-length: 6
  connection: close
  
  hello
  [closed]

An HTTP/1.0 request is persistent only with "Connection: keep-alive" (RFC
9112 section 9.3).

  $ printf 'GET /hello.txt HTTP/1.0\r\nHost: x\r\n\r\n' | send
  HTTP/1.1 200 OK
  [closed]
  $ printf 'GET /hello.txt HTTP/1.0\r\nHost: x\r\nConnection: Keep-Alive\r\n\r\n' |
  >   send
  HTTP/1.1 200 OK
  [idle]

Pipelined requests are answered in order on the same connection.

  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\n\r\nGET /index.html HTTP/1.1\r\nHost: x\r\n\r\n' |
  >   rawhttp send --port 8471 --idle 0.5 --settle 0.5 | grep -E '^(HTTP/|hello|index|\[)'
  HTTP/1.1 200 OK
  hello
  HTTP/1.1 200 OK
  index
  [idle]

An HTTP/1.1 request without Host is answered 400 (RFC 9112 section 3.2),
even with an absolute URI.

  $ printf 'GET /hello.txt HTTP/1.1\r\n\r\n' | send
  HTTP/1.1 400 Bad Request
  [closed]
  $ printf 'GET http://x/hello.txt HTTP/1.1\r\n\r\n' | send
  HTTP/1.1 400 Bad Request
  [closed]

A request with two Host headers is answered 400 too, in HTTP/1.0 as well: a
proxy in front that uses the other one would disagree with the server on the
virtual host.

  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\nHost: admin.test\r\n\r\n' |
  >   send
  HTTP/1.1 400 Bad Request
  [closed]
  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: admin.test\r\nHost: x\r\n\r\n' |
  >   send
  HTTP/1.1 400 Bad Request
  [closed]
  $ printf 'GET /hello.txt HTTP/1.0\r\nHost: x\r\nHost: admin.test\r\n\r\n' |
  >   send
  HTTP/1.1 400 Bad Request
  [closed]

So is a Host that is not a host name, an IPv4 address or an IPv6 address in
brackets, with an optional port. The connection is then closed.

  $ for h in 'x@admin.test' 'admin.test/x' 'x, admin.test' 'x:abc' '[::1' \
  >   '[admin.test]'; do
  >   printf 'GET /hello.txt HTTP/1.1\r\nHost: %s\r\n\r\n' "$h" | send; done
  HTTP/1.1 400 Bad Request
  [closed]
  HTTP/1.1 400 Bad Request
  [closed]
  HTTP/1.1 400 Bad Request
  [closed]
  HTTP/1.1 400 Bad Request
  [closed]
  HTTP/1.1 400 Bad Request
  [closed]
  HTTP/1.1 400 Bad Request
  [closed]

Percent-encoding is refused too: virtual hosts are chosen with the decoded
name, which a proxy in front of the server does not see. Here, a proxy that
protects admin.test would let these requests through.

  $ for h in '%61dmin.test' 'admin.test%0Aevil' 'x%41'; do
  >   printf 'GET / HTTP/1.1\r\nHost: %s\r\n\r\n' "$h" | send; done
  HTTP/1.1 400 Bad Request
  [closed]
  HTTP/1.1 400 Bad Request
  [closed]
  HTTP/1.1 400 Bad Request
  [closed]

These are valid.

  $ for h in '127.0.0.1:8471' '[::1]:8471' ''; do
  >   printf 'GET /hello.txt HTTP/1.1\r\nHost: %s\r\n\r\n' "$h" | send; done
  HTTP/1.1 200 OK
  [idle]
  HTTP/1.1 200 OK
  [idle]
  HTTP/1.1 200 OK
  [idle]

The host of a request target in absolute form, which replaces Host, is
checked in the same way.

  $ for t in 'http://%61dmin.test/' 'http://user@admin.test/' 'ftp://x/'; do
  >   printf 'GET %s HTTP/1.1\r\nHost: x\r\n\r\n' "$t" | send; done
  HTTP/1.1 400 Bad Request
  [closed]
  HTTP/1.1 400 Bad Request
  [closed]
  HTTP/1.1 400 Bad Request
  [closed]

An HTTP/1.0 request without Host (allowed in HTTP/1.0) goes, like a request
for an unknown name, to the catch-all host, not to the first <host> of the
configuration (admin.test here). The default health check of HAProxy is
such a request.

  $ printf 'GET /hello.txt HTTP/1.0\r\n\r\n' | send
  HTTP/1.1 200 OK
  [closed]
  $ printf 'OPTIONS / HTTP/1.0\r\n\r\n' | send
  HTTP/1.1 200 OK
  [closed]

Host names are case-insensitive (RFC 9110 section 4.2.3). The port is
ignored when matching a host.

  $ printf 'GET / HTTP/1.1\r\nHost: admin.test\r\n\r\n' | send
  HTTP/1.1 401 Unauthorized
  [idle]
  $ printf 'GET / HTTP/1.1\r\nHost: admin.test:8471\r\n\r\n' | send
  HTTP/1.1 401 Unauthorized
  [idle]

"ADMIN.TEST" reaches admin.test and its password too, not the catch-all host.

  $ printf 'GET / HTTP/1.1\r\nHost: ADMIN.TEST\r\n\r\n' | send
  HTTP/1.1 401 Unauthorized
  [idle]
  $ printf 'GET / HTTP/1.1\r\nHost: Admin.Test:8471\r\n\r\n' | send
  HTTP/1.1 401 Unauthorized
  [idle]
  $ printf 'GET http://ADMIN.TEST/ HTTP/1.1\r\nHost: x\r\n\r\n' | send
  HTTP/1.1 401 Unauthorized
  [idle]

The fully qualified form "admin.test." (final dot) names the same host. A
second final dot does not.

  $ printf 'GET / HTTP/1.1\r\nHost: admin.test.\r\n\r\n' | send
  HTTP/1.1 401 Unauthorized
  [idle]
  $ printf 'GET / HTTP/1.1\r\nHost: admin.test..\r\n\r\n' | send
  HTTP/1.1 200 OK
  [idle]

An absolute URI in the request line takes precedence over Host (RFC 9112
section 3.2.2).

  $ printf 'GET http://admin.test/ HTTP/1.1\r\nHost: x\r\n\r\n' | send
  HTTP/1.1 401 Unauthorized
  [idle]

The server still works.

  $ alive 8471
  HTTP/1.1 200 OK
