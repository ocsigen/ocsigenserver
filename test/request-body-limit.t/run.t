Request bodies decoded in memory (urlencoded POST parameters) are limited by
<maxrequestbodysizeinmemory>, 1 MiB by default.

  $ . ../server-test-helpers.sh
  $ mkdir -p log
  $ ../request-body-limit/test.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'stop_server $SERVER_PID' EXIT
  $ wait_server $SERVER_PID server.log local.sock local.cmd
  $ post () {
  >   curl --unix-socket local.sock -s -w ' (%{http_code})\n' \
  >     --data-binary "@$1" http://x/
  > }

Small parameters are decoded.

  $ printf 'a=1&b=2' > small
  $ post small
  2 parameter(s), 4 bytes (200)

Parameters larger than the former 8 kB default are accepted.

  $ printf 'x=' > large; head -c 102400 /dev/zero | tr '\0' a >> large
  $ post large
  1 parameter(s), 102401 bytes (200)

A body larger than the limit is rejected with 413, not 500, and the server logs
which options are involved.

  $ printf 'x=' > huge; head -c 1200000 /dev/zero | tr '\0' a >> huge
  $ post huge
  Error: Request Entity Too Large (413)
  $ for _ in $(seq 1 20); do
  >   grep -q 'Request body too large' server.log && break; sleep 0.1
  > done
  $ grep -o 'Request body too large.*' server.log
  Request body too large to be decoded in memory (see the <maxrequestbodysizeinmemory> and <netbuffersize> configuration options)
  $ grep -c 'Error while handling request' server.log || true
  0
