An order of the command pipe that fails does not stop the command pipe: the
later orders are still read. Self-contained POSIX sh.

  $ mkdir -p log data
  $ dune build ./test.exe 2>&1
  $ ./_build/default/test.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ i=0; while [ ! -e local.sock ] || [ ! -e local.cmd ]; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done

  $ echo 'test:fail' > local.cmd
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
  $ echo 'shutdown' > local.cmd
  $ wait $SERVER_PID; echo "exit $?"
  exit 0
  $ kill $WATCHDOG
