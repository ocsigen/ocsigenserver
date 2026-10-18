Static files support single byte-range requests (RFC 7233).

  $ mkdir www
  $ printf '0123456789ABCDEF' > www/data.txt
  $ ocsigenserver --serve www --port 8066 >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT

A full response advertises range support:

  $ curl -s -D headers -o /dev/null --max-time 10 --retry 20 --retry-delay 1 \
  >   --retry-connrefused http://127.0.0.1:8066/data.txt
  $ grep -io '^accept-ranges: bytes' headers
  accept-ranges: bytes

A byte range returns 206 with the right Content-Range and only those bytes:

  $ curl -s -D h -o body -w '%{http_code}\n' --max-time 10 \
  >   -H 'Range: bytes=0-3' http://127.0.0.1:8066/data.txt
  206
  $ grep -i '^content-range:' h | tr -d '\r'
  content-range: bytes 0-3/16
  $ cat body; echo
  0123

Serve mode compresses text files, and a compressed response does not offer
ranges, which are only served from the uncompressed file:

  $ curl -s -D h -o /dev/null --max-time 10 -H 'Accept-Encoding: gzip' \
  >   http://127.0.0.1:8066/data.txt
  $ grep -i '^content-encoding:' h | tr -d '\r'
  content-encoding: gzip
  $ grep -i '^accept-ranges:' h || echo 'no accept-ranges'
  no accept-ranges

but not a range: it is a part of the uncompressed file, so it goes out as is,
with its length. The body is shown with od, which keeps a failure readable if
it comes compressed.

  $ curl -s -D h -o body -w '%{http_code}\n' --max-time 10 \
  >   -H 'Range: bytes=0-3' -H 'Accept-Encoding: gzip' \
  >   http://127.0.0.1:8066/data.txt
  206
  $ grep -i '^content-encoding:' h || echo 'no content-encoding'
  no content-encoding
  $ grep -i '^content-length:' h | tr -d '\r'
  content-length: 4
  $ od -An -c body
     0   1   2   3

A suffix range counts from the end:

  $ curl -s -o body -w '%{http_code}\n' --max-time 10 \
  >   -H 'Range: bytes=-4' http://127.0.0.1:8066/data.txt
  206
  $ cat body; echo
  CDEF

An open-ended range runs to the last byte:

  $ curl -s -o body --max-time 10 -H 'Range: bytes=10-' \
  >   http://127.0.0.1:8066/data.txt
  $ cat body; echo
  ABCDEF

An unsatisfiable range gives 416:

  $ curl -s -o /dev/null -w '%{http_code}\n' --max-time 10 \
  >   -H 'Range: bytes=100-200' http://127.0.0.1:8066/data.txt
  416

When If-Range does not match the current ETag, the whole file is served:

  $ curl -s -o body -w '%{http_code}\n' --max-time 10 -H 'Range: bytes=0-3' \
  >   -H 'If-Range: "stale"' http://127.0.0.1:8066/data.txt
  200
  $ cat body; echo
  0123456789ABCDEF
