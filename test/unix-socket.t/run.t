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

A server holds a lock on local.sock.lock while it listens on local.sock. A
server killed without being able to stop leaves its socket, but its lock is
released, so the next server removes the socket and listens there:

  $ kill -9 $SERVER_PID; wait $SERVER_PID 2>/dev/null || true
  $ test -S local.sock && echo left
  left
  $ ../unix-socket/test.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ i=0; until curl -s --unix-socket local.sock -o /dev/null http://x/; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done
  $ curl --unix-socket local.sock -s http://example.org/dir/
  index

A socket that a server uses, as its lock tells, is kept: a second server fails
at once, and the first one still answers.

  $ ../unix-socket/test.exe >second.log 2>&1; echo "exit $?"
  exit 9
  $ grep -o 'Fatal - .*' second.log
  Fatal - lock ./local.sock: Address already in use
  $ curl --unix-socket local.sock -s http://example.org/dir/
  index

A program that takes no lock, such as an earlier version of the server, may
listen on the socket too. Here the lock file of the server that runs is
removed: the next server gets a new lock, but the socket is still kept.

  $ rm local.sock.lock
  $ ../unix-socket/test.exe >third.log 2>&1; echo "exit $?"
  exit 9
  $ grep -o 'Fatal - .*' third.log
  Fatal - connect ./local.sock: Address already in use
  $ curl --unix-socket local.sock -s http://example.org/dir/
  index
