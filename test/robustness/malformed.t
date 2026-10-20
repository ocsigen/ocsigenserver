Malformed request lines and header sections (RFC 9112 sections 2 to 5).
Answers marked BUG show the current behaviour, with the expected one in the
text. [closed] with no status line means the server closed the connection
without answering. For most malformed requests, RFC 9112 recommends a 400
response, but closing is safe, so it is not marked as a bug. It is marked as
one where a status code is required.

  $ . ./fixture/helpers.sh
  $ start_server
  $ send () { rawhttp send --port "$PORT" --idle 0.5 --status; }
  $ many () { head -c "$1" /dev/zero | tr '\0' "$2"; }

BUG: a line that is not a request line gets no answer and the connection
stays open. Expected: 400 or [closed]. Such a connection is only released by
the <timeout> of the server (30 seconds by default).

  $ printf 'garbage\r\n\r\n' | send
  [idle]
  $ printf 'GET\r\nHost: x\r\n\r\n' | send
  [idle]
  $ printf 'GET /hello.txt\r\n\r\n' | send
  [idle]

Wrong versions and syntax are rejected, and so are bare LF line ends
(accepting them is optional, section 2.2).

  $ printf 'GET /hello.txt HTTP/2.0\r\nHost: x\r\n\r\n' | send
  [closed]
  $ printf 'GET /hello.txt HTTP/9.9\r\nHost: x\r\n\r\n' | send
  [closed]
  $ printf 'GET /hello.txt HTTP/1.1 extra\r\nHost: x\r\n\r\n' | send
  [closed]
  $ printf 'GET  /hello.txt  HTTP/1.1\r\nHost: x\r\n\r\n' | send
  [closed]
  $ printf 'GET /hello.txt HTTP/1.1\nHost: x\n\n' | send
  [closed]
  $ printf 'GET hello.txt HTTP/1.1\r\nHost: x\r\n\r\n' | send
  [closed]

Header fields: malformed lines are rejected.

  $ printf 'GET /hello.txt HTTP/1.1\r\n Host: x\r\n\r\n' | send
  [closed]
  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\nX-A: a\rb\r\n\r\n' | send
  [closed]
  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\n: empty\r\n\r\n' | send
  [closed]
  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\nX(A): a\r\n\r\n' | send
  [closed]

BUG: a request with whitespace between a field name and the colon must be
rejected with a 400 response (section 5.1). Expected: 400.

  $ printf 'GET /hello.txt HTTP/1.1\r\nHost : x\r\n\r\n' | send
  [closed]

BUG: a header line without colon and an obsolete line folding (section 5.2)
get no answer and the connection stays open, until the <timeout> of the
server. Expected: 400 or [closed], or, for the folding, its replacement by a
space and a normal answer.

  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\nNoColon\r\n\r\n' | send
  [idle]
  $ printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\nX-A: a\r\n b\r\n\r\n' | send
  [idle]

BUG: a request target longer than the server accepts must be answered with
414 (section 3), and too large header fields with a 4xx status code such as
431 (RFC 9110 section 5.4). Expected: 414 for the long request line, 431 for
the long field and for the many fields.

  $ { printf 'GET /'; many 100000 a; printf ' HTTP/1.1\r\nHost: x\r\n\r\n'; } | send
  [closed]
  $ { printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\nX-A: '; many 100000 a;
  >   printf '\r\n\r\n'; } | send
  [closed]
  $ { printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\n';
  >   awk 'BEGIN { for (i = 0; i < 20000; i++) printf "X-A: a\r\n" }';
  >   printf '\r\n'; } | send
  [closed]

The server still works.

  $ alive
  HTTP/1.1 200 OK
