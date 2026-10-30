When Revproxy gets no response from its target, the client gets a 502, and
the error log says why, without the path and query of the request. Nothing
listens on port 1 of the target. Self-contained POSIX sh.

  $ . ../server-test-helpers.sh
  $ free_ports 1
  $ ocsigenserver --reverse-proxy http://127.0.0.1:1 --port $PORT1 \
  >   >proxy.log 2>&1 &
  $ PROXY_PID=$!
  $ trap 'kill $PROXY_PID 2>/dev/null' EXIT
  $ wait_server $PROXY_PID proxy.log http://127.0.0.1:$PORT1/
  $ curl -s -o /dev/null -w '%{http_code}\n' \
  >   "http://127.0.0.1:$PORT1/a?token=x"
  502

The request of wait_server logged the same line, hence sort -u:

  $ grep -o 'Cannot get a response.*' proxy.log | sort -u
  Cannot get a response from http://127.0.0.1:1: Unix.Unix_error(Unix.ECONNREFUSED, "connect", "")
