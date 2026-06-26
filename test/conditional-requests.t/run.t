Static files are served with validators (ETag and Last-Modified), and
conditional requests are answered with 304 Not Modified.

  $ mkdir www
  $ printf 'cacheable content' > www/f.txt
  $ ocsigenserver --serve www --port 8065 >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT

A normal request succeeds and carries both validators:

  $ curl -s -D headers -o /dev/null --retry 20 --retry-delay 1 \
  >   --retry-connrefused http://127.0.0.1:8065/f.txt
  $ grep -io '^etag:' headers
  etag:
  $ grep -io '^last-modified:' headers
  last-modified:

Re-requesting with the returned ETag yields 304 Not Modified with an empty body:

  $ ETAG=$(grep -i '^etag:' headers | sed 's/^[^:]*: //' | tr -d '\r')
  $ curl -s -o body -w '%{http_code}\n' -H "If-None-Match: $ETAG" \
  >   http://127.0.0.1:8065/f.txt
  304
  $ wc -c < body
  0

Re-requesting with the returned Last-Modified date also yields 304:

  $ LM=$(grep -i '^last-modified:' headers | sed 's/^[^:]*: //' | tr -d '\r')
  $ curl -s -o /dev/null -w '%{http_code}\n' -H "If-Modified-Since: $LM" \
  >   http://127.0.0.1:8065/f.txt
  304

A non-matching ETag still serves the file:

  $ curl -s -o /dev/null -w '%{http_code}\n' -H 'If-None-Match: W/"nomatch"' \
  >   http://127.0.0.1:8065/f.txt
  200
