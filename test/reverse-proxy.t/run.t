Forward every request to another server with a single command, with no
configuration file.

Start an upstream server that serves a directory:

  $ . ../server-test-helpers.sh
  $ free_ports 2
  $ mkdir upstream
  $ printf 'hello from upstream' > upstream/hello.txt
  $ ocsigenserver --serve upstream --port $PORT1 >upstream.log 2>&1 &
  $ UPSTREAM_PID=$!
  $ trap 'kill $UPSTREAM_PID $PROXY_PID 2>/dev/null' EXIT
  $ wait_server $UPSTREAM_PID upstream.log http://127.0.0.1:$PORT1/hello.txt

The upstream answers:

  $ curl -s --max-time 10 http://127.0.0.1:$PORT1/hello.txt; echo
  hello from upstream

Start a reverse proxy in front of it:

  $ ocsigenserver --reverse-proxy http://127.0.0.1:$PORT1 --port $PORT2 \
  >   >proxy.log 2>&1 &
  $ PROXY_PID=$!
  $ wait_server $PROXY_PID proxy.log http://127.0.0.1:$PORT2/hello.txt

A request to the proxy is forwarded to the upstream and its answer is relayed:

  $ curl -s --max-time 10 http://127.0.0.1:$PORT2/hello.txt; echo
  hello from upstream

--max-request-body-size takes the sizes of the configuration file, and is only
valid with --reverse-proxy:

  $ ocsigenserver --reverse-proxy http://127.0.0.1:$PORT1 \
  >   --max-request-body-size -1 2>err.log
  [2]
  $ sed 's/^.*\] //' err.log
  --max-request-body-size: -1 is not a size (such as 100MB, 1GiB or infinity)
  $ ocsigenserver --serve upstream --max-request-body-size 1MB 2>err.log
  [2]
  $ sed 's/^.*\] //' err.log
  --max-request-body-size is only valid with --reverse-proxy
