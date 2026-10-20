The shutdown order of the command pipe. POSIX sh.

  $ . ../server-test-helpers.sh
  $ mkdir -p log
  $ ../shutdown-command/test.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ wait_server $SERVER_PID server.log local.sock local.cmd

A delay that is not a number is reported, and the server keeps running:

  $ send_command $SERVER_PID local.cmd 'shutdown soon'
  $ for _ in $(seq 1 20); do
  >   grep -q 'is not a delay' log/errors.log 2>/dev/null && break
  >   sleep 0.5
  > done
  $ grep -o 'shutdown: "soon" is not a delay in seconds' log/errors.log
  shutdown: "soon" is not a delay in seconds
  $ curl --unix-socket local.sock -s -o /dev/null -w '%{http_code}\n' http://x/
  404

"shutdown notimeout" stops the server:

  $ send_command $SERVER_PID local.cmd 'shutdown notimeout'
  $ wait $SERVER_PID; echo "exit $?"
  exit 0
