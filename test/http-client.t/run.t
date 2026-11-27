A request that an application sends with Cohttp_lwt_unix.Client, and that
fails, fails for that application only: the server keeps running. cohttp
raises the failure again outside of any promise, which stopped the server
(cohttp#1153).

  $ . ../server-test-helpers.sh
  $ free_ports 1
  $ ../revproxy-forwarding/raw.exe $PORT1 >raw.log 2>&1 &
  $ RAW=$!
  $ mkdir -p log
  $ ../http-client/test.exe $PORT1 >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'stop_server $SERVER_PID; kill $RAW; wait' EXIT
  $ wait_server $RAW raw.log http://127.0.0.1:$PORT1/length
  $ wait_server $SERVER_PID server.log local.sock local.cmd
  $ get () {
  >   curl --unix-socket local.sock -s --max-time 10 "http://x/$1"
  > }

A target that does not answer in HTTP:

  $ get malformed
  failed: Failure

A request body that cannot be read:

  $ get unreadable-body
  failed: Exit

The server still answers, and the failures were not raised elsewhere:

  $ get alive
  no request
  $ grep -c Uncaught server.log
  0
  [1]
