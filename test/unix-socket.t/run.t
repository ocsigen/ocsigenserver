A server listening on a Unix-domain socket, typically behind a reverse proxy,
has no port of its own: the absolute URLs it builds use the default port.

  $ mkdir -p log data www/dir
  $ echo index > www/dir/index.html
  $ dune build ./test.exe 2>&1
  $ dune exec -- ./test.exe >server.log 2>&1 &
  $ trap 'echo shutdown > local.cmd 2>/dev/null; wait' EXIT
  $ i=0; while [ ! -e local.sock ] || [ ! -e local.cmd ]; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done

The redirection of a directory to its URL with a slash has no port 0.

  $ curl --unix-socket local.sock -s -o /dev/null \
  >   -w '%{http_code} %{redirect_url}\n' http://example.org/dir
  301 http://example.org/dir/
  $ curl --unix-socket local.sock -s http://example.org/dir/
  index
