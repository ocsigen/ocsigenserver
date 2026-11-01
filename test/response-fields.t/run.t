A header or a cookie that would break the response, or let a client inject
headers through a value, is not sent: the response is replaced by an error.

  $ . ../server-test-helpers.sh
  $ mkdir -p log
  $ ../response-fields/test.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'stop_server $SERVER_PID' EXIT
  $ wait_server $SERVER_PID server.log local.sock local.cmd
  $ get () {
  >   curl --unix-socket local.sock -s -i --max-time 10 "http://x/$1" \
  >   | grep -iE '^(HTTP/|x-|set-cookie:)' | tr -d '\r'
  > }

Valid headers and cookies are sent, including a tab or bytes that are not
ASCII in a value, and a cookie name with the characters of Eliom's names,
which contain the path of the site:

  $ get tab-in-header
  HTTP/1.1 200 OK
  x-a: a	b
  $ get non-ascii-in-header
  HTTP/1.1 200 OK
  x-a: été
  $ get eliom-cookie-name
  HTTP/1.1 200 OK
  set-cookie: eliomdatasession|0|/my site/é/|=v; path=/
  $ get space-in-cookie-value
  HTTP/1.1 200 OK
  set-cookie: c=a b; path=/

A secure cookie has the Secure attribute, even through HTTP, as the server
may be behind a proxy that terminates TLS:

  $ get secure-cookie
  HTTP/1.1 200 OK
  set-cookie: c=v; path=/; secure

A CR or LF in a header, or in a cookie, would add a header. Other control
characters are refused too:

  $ get crlf-in-header
  HTTP/1.1 500 Internal Server Error
  $ get lf-in-header
  HTTP/1.1 500 Internal Server Error
  $ get nul-in-header
  HTTP/1.1 500 Internal Server Error
  $ get del-in-header
  HTTP/1.1 500 Internal Server Error
  $ get crlf-in-cookie
  HTTP/1.1 500 Internal Server Error

A space in a header name, a [;] in a cookie value or a [=] in a cookie name
would change the meaning of the header:

  $ get space-in-header-name
  HTTP/1.1 500 Internal Server Error
  $ get semicolon-in-cookie
  HTTP/1.1 500 Internal Server Error
  $ get equals-in-cookie-name
  HTTP/1.1 500 Internal Server Error

The cookies of an error response are sent, and checked too:

  $ get error-with-cookie
  HTTP/1.1 403 Forbidden
  set-cookie: c=v; path=/
  $ get error-with-bad-cookie
  HTTP/1.1 500 Internal Server Error

The log names what was not sent, without its value:

  $ grep -o 'Response replaced.*' log/errors.log
  Response replaced by a 500 error: the value of header x-a has a forbidden character
  Response replaced by a 500 error: the value of header x-a has a forbidden character
  Response replaced by a 500 error: the value of header x-a has a forbidden character
  Response replaced by a 500 error: the value of header x-a has a forbidden character
  Response replaced by a 500 error: the value of cookie c has a forbidden character
  Response replaced by a 500 error: the header name "x a" is not a token
  Response replaced by a 500 error: the value of cookie c has a forbidden character
  Response replaced by a 500 error: the cookie name "a=b" has a forbidden character
  Response replaced by a 500 error: the value of cookie c has a forbidden character
  $ grep -c injected log/errors.log
  0
  [1]

The body of a replaced response is released: a file response does not leave
its file open.

  $ echo content > file.txt
  $ count () { curl --unix-socket local.sock -s --max-time 10 http://x/open-files; }
  $ before=$(count)
  $ for i in $(seq 20); do get file-with-bad-cookie > /dev/null; done
  $ after=$(count)
  $ test $((after - before)) -lt 10 && echo released
  released
