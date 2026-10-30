A server listening on a Unix-domain socket, typically behind a reverse proxy,
has no port of its own: the absolute URLs it builds use the default port.

  $ . ../server-test-helpers.sh
  $ mkdir -p log www/dir
  $ echo index > www/dir/index.html
  $ ../unix-socket/test.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'stop_server $SERVER_PID' EXIT
  $ wait_server $SERVER_PID server.log local.sock local.cmd

The redirection of a directory to its URL with a slash has no port 0.

  $ curl --unix-socket local.sock -s -o /dev/null \
  >   -w '%{http_code} %{redirect_url}\n' http://example.org/dir
  301 http://example.org/dir/
  $ curl --unix-socket local.sock -s http://example.org/dir/
  index

A server killed without being able to stop leaves its socket. Nothing listens
on it any more, so the next server removes it and listens there:

  $ kill -9 $SERVER_PID; wait $SERVER_PID 2>/dev/null || true
  $ test -S local.sock && echo left
  left
  $ ../unix-socket/test.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ i=0; until curl -s --unix-socket local.sock -o /dev/null http://x/; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done
  $ curl --unix-socket local.sock -s http://example.org/dir/
  index

A socket on which a server listens is kept: a second server fails, and the
first one still answers.

  $ ../unix-socket/test.exe >second.log 2>&1; echo "exit $?"
  exit 9
  $ grep -o 'Fatal - .*' second.log
  Fatal - bind ./local.sock: Address already in use
  $ curl --unix-socket local.sock -s http://example.org/dir/
  index
