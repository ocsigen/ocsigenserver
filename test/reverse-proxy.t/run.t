Forward every request to another server with a single command, with no
configuration file.

Start an upstream server that serves a directory:

  $ mkdir upstream
  $ printf 'hello from upstream' > upstream/hello.txt
  $ ocsigenserver --serve upstream --port 8071 >upstream.log 2>&1 &
  $ UPSTREAM_PID=$!
  $ trap 'kill $UPSTREAM_PID $PROXY_PID 2>/dev/null' EXIT

Wait for the upstream to answer:

  $ curl -s --retry 20 --retry-delay 1 --retry-connrefused \
  >   http://127.0.0.1:8071/hello.txt
  hello from upstream (no-eol)

Start a reverse proxy in front of it:

  $ ocsigenserver --reverse-proxy http://127.0.0.1:8071 --port 8072 \
  >   >proxy.log 2>&1 &
  $ PROXY_PID=$!

A request to the proxy is forwarded to the upstream and its answer is relayed:

  $ curl -s --retry 20 --retry-delay 1 --retry-connrefused \
  >   http://127.0.0.1:8072/hello.txt
  hello from upstream (no-eol)
