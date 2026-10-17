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

With a configuration that gives neither a log directory nor syslog, a server
built with no default log directory logs to the terminal too. An empty log
directory does the same here.

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
