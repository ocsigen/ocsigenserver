Connection management and the Host header (RFC 9112 sections 3.2 and 9).
Answers marked BUG show the current behaviour, with the expected one in the
text.

  $ . ./fixture/helpers.sh
  $ make_certificate
  $ start_server --tls
  $ send () { rawhttp send --port "$PORT" --idle 0.5 --status; }

With "Connection: close", the server closes the connection after the
response, which says "Connection: close" too (RFC 9112 section 9.6). A
request that follows on the same connection is not answered. Connection
options are case-insensitive, and the field may be a list.

  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n' |
  >   rawhttp send --port "$PORT" --idle 0.5
  HTTP/1.1 200 OK
  content-type: text/plain; charset=utf-8
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
  >   rawhttp send --port "$TLS_PORT" --tls --idle 0.5
  HTTP/1.1 200 OK
  content-type: text/plain; charset=utf-8
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
  >   rawhttp send --port "$PORT" --idle 0.5 | grep -E '^(HTTP/|hello|index|\[)'
  HTTP/1.1 200 OK
  hello
  HTTP/1.1 200 OK
  index
  [idle]

BUG: an HTTP/1.1 request without Host must be answered 400 (RFC 9112
section 3.2). It goes to the first <host> of the configuration instead.

  $ printf 'GET /hello.txt HTTP/1.1\r\n\r\n' | send
  HTTP/1.1 401 Unauthorized
  [idle]

BUG: a request with two Host headers must be answered 400 (RFC 9112 section
3.2). The last one is used instead. A proxy in front that uses the first one
would then disagree with the server on the virtual host.

  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\nHost: admin.test\r\n\r\n' |
  >   send
  HTTP/1.1 401 Unauthorized
  [idle]
  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: admin.test\r\nHost: x\r\n\r\n' |
  >   send
  HTTP/1.1 200 OK
  [idle]

QUESTION: an HTTP/1.0 request without Host (allowed in HTTP/1.0) goes to the
first <host> of the configuration (admin.test here, which asks for a
password), not to the catch-all host. The manual does not say which host
gets such requests.

  $ printf 'GET /hello.txt HTTP/1.0\r\n\r\n' | send
  HTTP/1.1 401 Unauthorized
  [closed]

Host names are case-insensitive (RFC 9110 section 4.2.3). The port is
ignored when matching a host.

  $ printf 'GET / HTTP/1.1\r\nHost: admin.test\r\n\r\n' | send
  HTTP/1.1 401 Unauthorized
  [idle]
  $ printf 'GET / HTTP/1.1\r\nHost: %s\r\n\r\n' "admin.test:$PORT" | send
  HTTP/1.1 401 Unauthorized
  [idle]

BUG: the virtual host is chosen with a case-sensitive comparison, so
"ADMIN.TEST" does not reach admin.test and its password, but the catch-all
host. Expected: 401, as for "admin.test".

  $ printf 'GET / HTTP/1.1\r\nHost: ADMIN.TEST\r\n\r\n' | send
  HTTP/1.1 200 OK
  [idle]

QUESTION: the fully qualified form "admin.test." (final dot) does not match
admin.test either.

  $ printf 'GET / HTTP/1.1\r\nHost: admin.test.\r\n\r\n' | send
  HTTP/1.1 200 OK
  [idle]

An absolute URI in the request line takes precedence over Host (RFC 9112
section 3.2.2).

  $ printf 'GET http://admin.test/ HTTP/1.1\r\nHost: x\r\n\r\n' | send
  HTTP/1.1 401 Unauthorized
  [idle]

The server still works.

  $ alive
  HTTP/1.1 200 OK
