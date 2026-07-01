Byte-range (partial) requests.

NOTE: range requests are not implemented in the current cohttp-based server
(there is a "TODO: equivalent of Ocsigen_range" in ocsigen_cohttp.ml). The
server therefore ignores the Range header and replies with the full
representation and a 200. These tests pin that current behaviour so the
limitation is visible and so that adding real range support is an obvious,
intentional change to this file.

  $ . ../server-test-helpers.sh
  $ printf '0123456789' > digits.txt
  $ run_server ../range_requests_server.exe

No Accept-Ranges header is advertised.

  $ curl_ digits.txt | grep -ic '^accept-ranges:' || true
  0

A Range request is ignored: the full body is returned with a 200 (not 206).

  $ curl_ digits.txt -r 0-3
  HTTP/1.1 200 OK
  content-type: text/plain
  server: Ocsigen
  content-length: 10
  
  0123456789

An out-of-bounds range is likewise ignored rather than answered with 416.

  $ curl_ digits.txt -r 50-60 | head -1
  HTTP/1.1 200 OK
