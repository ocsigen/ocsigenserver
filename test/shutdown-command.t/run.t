The shutdown order of the command pipe. Self-contained POSIX sh.

  $ mkdir -p log
  $ dune build ./test.exe 2>&1
  $ ./_build/default/test.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ i=0; while [ ! -e local.sock ] || [ ! -e local.cmd ]; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done

A delay that is not a number is reported, and the server keeps running:

  $ echo 'shutdown soon' > local.cmd
  $ for _ in $(seq 1 20); do
  >   grep -q 'is not a delay' log/errors.log 2>/dev/null && break
  >   sleep 0.5
  > done
  $ grep -o 'shutdown: "soon" is not a delay in seconds' log/errors.log
  shutdown: "soon" is not a delay in seconds
  $ curl --unix-socket local.sock -s -o /dev/null -w '%{http_code}\n' http://x/
  404

"shutdown notimeout" stops the server:

  $ echo 'shutdown notimeout' > local.cmd
  $ wait $SERVER_PID; echo "exit $?"
  exit 0
