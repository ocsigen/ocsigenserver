In daemon mode (-d), the command returns once the detached process is
initialised and listens. If the server does not start, the command fails with
its error and exit code, says nothing of a detached process and writes no pid
file, although the error happens in the detached process, after its logs are
open.

  $ . ../server-test-helpers.sh
  $ free_ports 1
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
detached process in the pid file. The server listens as soon as the command
returns:

  $ sed 's|<cors foo="1"/>||; s|</host>|</host><commandpipe>local.cmd</commandpipe>|' \
  >   attr.conf > good.conf
  $ ocsigenserver -c good.conf -d -p server.pid >out.log 2>&1; echo "exit $?"
  exit 0
  $ trap 'stop_server $(cat server.pid)' EXIT
  $ sed 's/^.*: ocsigen:main: *//' out.log | sed "s/$(cat server.pid)/PID/"
  Process PID detached
  $ curl -s -o /dev/null -w '%{http_code}\n' --max-time 10 \
  >   http://127.0.0.1:$PORT1/
  404

A port already in use, here by the server above, makes the command fail too:

  $ mkdir -p log2
  $ sed 's|<logdir>log|<logdir>log2|; s|<commandpipe>local.cmd</commandpipe>||' \
  >   good.conf > busy.conf
  $ ocsigenserver -c busy.conf -d -p busy.pid >out.log 2>&1; echo "exit $?"
  exit 9
  $ grep -o 'Fatal - .*\|detached' out.log | sed "s/:$PORT1:/:PORT:/"
  Fatal - bind 127.0.0.1:PORT: Address already in use
  $ test -e busy.pid
  [1]
