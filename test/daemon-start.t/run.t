In daemon mode (-d), the command returns once the detached process is
initialised. If the server does not start, the command fails with its error
and exit code, says nothing of a detached process and writes no pid file,
although the error happens in the detached process, after its logs are open.

  $ . ../server-test-helpers.sh
  $ free_ports 2
  $ mkdir -p log
  $ cat > attr.conf <<CONF
  > <ocsigen><server>
  > <port>127.0.0.1:$PORT1</port>
  > <logdir>log</logdir>
  > <extension findlib-package="ocsigenserver.ext.cors"/>
  > <host hostfilter="*"><cors foo="1"/></host>
  > </server></ocsigen>
  > CONF
  $ ocsigenserver -c attr.conf -d -p server.pid >out.log 2>&1; echo "exit $?"
  exit 50
  $ grep -o 'Fatal - .*\|detached' out.log
  Fatal - Error in configuration file: Unexpected attribute foo in tag cors
  $ test -e server.pid
  [1]

The detached process also logs the error in its own logs:

  $ grep -o 'Fatal - .*' log/errors.log
  Fatal - Error in configuration file: Unexpected attribute foo in tag cors

So when it cannot open its logs:

  $ touch notadir
  $ sed 's|<logdir>log|<logdir>notadir|; s|<cors foo="1"/>||' attr.conf \
  >   > nolog.conf
  $ ocsigenserver -c nolog.conf -d -p server.pid >out.log 2>&1; echo "exit $?"
  exit 50
  $ grep -o 'Fatal - .*\|detached' out.log
  Fatal - Error in configuration file: can't open log file notadir/access.log: Not a directory
  $ test -e server.pid
  [1]

When the server starts, the command says so and writes the process id of the
detached process in the pid file:

  $ sed 's|<cors foo="1"/>||; s|</host>|</host><commandpipe>local.cmd</commandpipe>|' \
  >   attr.conf > good.conf
  $ ocsigenserver -c good.conf -d -p server.pid >out.log 2>&1; echo "exit $?"
  exit 0
  $ trap 'stop_server $(cat server.pid)' EXIT
  $ sed 's/^.*: ocsigen:main: *//' out.log | sed "s/$(cat server.pid)/PID/"
  Process PID detached
  $ wait_server $(cat server.pid) log/errors.log local.cmd \
  >   http://127.0.0.1:$PORT1/

A pid file that cannot be written makes the command fail before the server
starts. This server would listen on a free port, so that nothing else makes
it fail:

  $ sed "s|127.0.0.1:$PORT1|127.0.0.1:$PORT2|; s|<commandpipe>local.cmd</commandpipe>||" \
  >   good.conf > free.conf
  $ ocsigenserver -c free.conf -d -p missing/other.pid >out.log 2>&1
  [9]
  $ grep -o 'Fatal - .*\|detached' out.log
  Fatal - Unix.Unix_error(Unix.ENOENT, "open", "missing/other.pid")
  $ curl -s -o /dev/null --max-time 10 http://127.0.0.1:$PORT2/
  [7]

A server that does not start, in daemon mode or not, leaves the pid file of
the server that runs as it was:

  $ cp server.pid saved.pid
  $ ocsigenserver -c attr.conf -d -p server.pid >out.log 2>&1
  [50]
  $ ocsigenserver -c attr.conf -p server.pid >out.log 2>&1
  [50]
  $ cmp server.pid saved.pid && echo same
  same

Interrupting the command (Ctrl-C) while the server starts stops the server,
and the command fails. Here the server waits to open its access log, a named
pipe that nothing reads:

  $ mkdir -p slowlog && mkfifo slowlog/access.log
  $ sed 's|<logdir>log|<logdir>slowlog|; s|<commandpipe>local.cmd</commandpipe>||' \
  >   good.conf > slow.conf
  $ ocsigenserver -c slow.conf -d -p slow.pid >out.log 2>&1 &
  $ COMMAND_PID=$!
  $ i=0; until CHILD_PID=$(pgrep -P $COMMAND_PID); do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done
  $ test -n "$CHILD_PID" && echo started
  started
  $ kill -INT $COMMAND_PID; wait $COMMAND_PID
  [130]
  $ grep -o 'Fatal - .*\|detached' out.log
  Fatal - interrupted: the server is stopped.
  $ i=0; while kill -0 $CHILD_PID 2>/dev/null; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done
  $ kill -0 $CHILD_PID 2>/dev/null
  [1]
  $ test -e slow.pid
  [1]

Without -d, a server interrupted while it starts removes the pid file that it
created:

  $ ocsigenserver -c slow.conf -p new.pid >out.log 2>&1 &
  $ SLOW_PID=$!
  $ i=0; until [ -e new.pid ]; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done
  $ test -e new.pid && echo created
  created
  $ kill -INT $SLOW_PID; wait $SLOW_PID
  $ test -e new.pid
  [1]
