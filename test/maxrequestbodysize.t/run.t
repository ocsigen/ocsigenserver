Request bodies are limited by <maxrequestbodysize>, here 1000 bytes. Reading
more gives a 413.

  $ . ../server-test-helpers.sh
  $ mkdir -p log
  $ ../maxrequestbodysize/test.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'stop_server $SERVER_PID' EXIT
  $ wait_server $SERVER_PID server.log local.sock local.cmd
  $ head -c 500 /dev/zero | tr '\0' a > small
  $ head -c 2000 /dev/zero | tr '\0' a > big
  $ post () {
  >   curl --unix-socket local.sock -s -w ' (%{http_code})\n' "$@"
  > }

A body within the limit is read:

  $ post --data-binary @small http://x/
  500 bytes (200)

A body whose Content-Length is above the limit is refused at its first read,
before any of it is kept:

  $ post --data-binary @big http://x/
  Error: Request Entity Too Large (413)

A chunked body, whose length is not announced, is refused once it goes
beyond the limit:

  $ post -H 'Transfer-Encoding: chunked' --data-binary @big http://x/
  Error: Request Entity Too Large (413)
  $ post -H 'Transfer-Encoding: chunked' --data-binary @small http://x/
  500 bytes (200)

The limit also applies to the POST parameters, urlencoded or multipart:

  $ post --data-urlencode x@small http://x/params
  1 parameter(s), 501 bytes (200)
  $ post --data-urlencode x@big http://x/params
  Error: Request Entity Too Large (413)
  $ post -F x='<small' http://x/params
  1 parameter(s), 501 bytes (200)
  $ post -F x='<big' http://x/params
  Error: Request Entity Too Large (413)

The server logs each refusal, with the options involved:

  $ grep -c 'Request body too large (see the <maxrequestbodysize> and <maxuploadfilesize> configuration options)' server.log
  4
  $ grep -c 'Error while handling request' server.log || true
  0
