free_ports sets PORT1, ..., PORTN to distinct TCP ports, free on this machine.

  $ . ../server-test-helpers.sh
  $ free_ports 3
  $ printf '%s\n' "$PORT1" "$PORT2" "$PORT3" | sort -u | grep -c '^[0-9][0-9]*$'
  3
  $ for p in $PORT1 $PORT2 $PORT3; do
  >   [ "$p" -gt 0 ] && [ "$p" -lt 65536 ] || echo "bad port $p"; done

A server can listen on them:

  $ mkdir www
  $ echo hello > www/index.html
  $ ocsigenserver --serve www --port $PORT2 >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT
  $ wait_server $SERVER_PID server.log http://127.0.0.1:$PORT2/
  $ curl -s --max-time 10 http://127.0.0.1:$PORT2/index.html
  hello

The number of ports must be positive:

  $ free_ports 0
  usage: free_port N, with N > 0
  [1]
