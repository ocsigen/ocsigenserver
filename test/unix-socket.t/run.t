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
