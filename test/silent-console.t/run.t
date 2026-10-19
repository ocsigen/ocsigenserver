Silent mode (-s) and daemon mode (-d) redirect the standard output and error
to /dev/null. When the logs go there, the server refuses to start rather than
log nothing at all, errors included.

The one-command modes always log to the terminal.

  $ mkdir www
  $ ocsigenserver --serve www -s >out.log 2>&1; echo "exit $?"
  exit 2
  $ sed 's/^.*\[ERROR\] //' out.log
  -s and -d cannot be combined with serve mode or --reverse-proxy, which log to the terminal
  $ ocsigenserver --reverse-proxy http://127.0.0.1:1 -d >out.log 2>&1; echo "exit $?"
  exit 2
  $ sed 's/^.*\[ERROR\] //' out.log
  -s and -d cannot be combined with serve mode or --reverse-proxy, which log to the terminal

With neither a log directory nor syslog, in the configuration file or here
with Ocsigen.Server.start, the logs go to the terminal too. The program
installs no Logs reporter: the server reports its errors on the terminal
anyway.

  $ dune build ./test.exe 2>&1
  $ ./_build/default/test.exe -s >out.log 2>&1; echo "exit $?"
  exit 50
  $ grep -o 'Fatal - .*' out.log
  Fatal - silent mode (-s) and daemon mode (-d) need log files or syslog, but the logs go to the terminal, which these modes redirect to /dev/null. Set a log directory or syslog (<logdir> or <syslog> in the configuration file, ~logdir or ~syslog_facility for Ocsigen.Server.start), or drop -s and -d.

With -d, the server is refused before it detaches, so the command fails at
once.

  $ ./_build/default/test.exe -d >out.log 2>&1; echo "exit $?"
  exit 50
  $ grep -o 'Fatal - .*' out.log
  Fatal - silent mode (-s) and daemon mode (-d) need log files or syslog, but the logs go to the terminal, which these modes redirect to /dev/null. Set a log directory or syslog (<logdir> or <syslog> in the configuration file, ~logdir or ~syslog_facility for Ocsigen.Server.start), or drop -s and -d.
  $ grep -c detached out.log
  0
  [1]
  $ test -e server.pid
  [1]

With a log directory, the server detaches. The detached process is silent, but
the command tells its process id.

  $ mkdir log
  $ ./_build/default/test.exe -d log >out.log 2>&1; echo "exit $?"
  exit 0
  $ trap 'kill $(cat server.pid) 2>/dev/null' EXIT
  $ sed 's/^.*: ocsigen:main: *//' out.log | sed "s/$(cat server.pid)/PID/"
  Process PID detached
  $ i=0; while [ ! -e local.sock ]; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done
  $ ls local.sock
  local.sock
