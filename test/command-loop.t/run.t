An order of the command pipe that fails does not stop the command pipe: the
later orders are still read. POSIX sh.

  $ . ../server-test-helpers.sh
  $ mkdir -p log
  $ ../command-loop/test.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ wait_server $SERVER_PID server.log local.sock local.cmd

  $ send_command $SERVER_PID local.cmd test:fail
  $ for _ in $(seq 1 20); do
  >   grep -q 'this order always fails' log/errors.log 2>/dev/null && break
  >   sleep 0.5
  > done
  $ grep -o 'Command .* failed: .*' log/errors.log
  Command "test:fail" failed: Failure("this order always fails")

The next order is still obeyed. If it is not, a watchdog stops the server
after 10 seconds, so that the test fails instead of hanging:

  $ (sleep 10; kill $SERVER_PID) >/dev/null 2>&1 &
  $ WATCHDOG=$!
  $ send_command $SERVER_PID local.cmd shutdown
  $ wait $SERVER_PID; echo "exit $?"
  exit 0
  $ kill $WATCHDOG
