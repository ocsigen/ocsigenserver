<logdir> and <syslog> are mutually exclusive: the server refuses to start
when both are given. The pid file is in a missing directory: a server that
passes the check stops there, before opening any log, rather than run.

  $ cat > conf.xml <<EOF
  > <ocsigen><server>
  >   <port>8098</port>
  >   <logdir>log</logdir>
  >   <syslog>local0</syslog>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c conf.xml -p missing/server.pid >out.log 2>&1; echo "exit $?"
  exit 50
  $ grep -o 'Fatal - .*' out.log
  Fatal - a log directory and syslog are mutually exclusive: the logs go either to files or to syslog. Set only one of them (<logdir> or <syslog> in the configuration file, ~logdir or ~syslog_facility for Ocsigen.Server.start).

An empty <logdir> is no log directory, so it is allowed with <syslog>.

  $ cat > conf.xml <<EOF
  > <ocsigen><server>
  >   <port>8098</port>
  >   <logdir></logdir>
  >   <syslog>local0</syslog>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c conf.xml -p missing/server.pid >out.log 2>&1; echo "exit $?"
  exit 9
  $ grep -o 'Fatal - .*' out.log
  Fatal - Unix.Unix_error(Unix.ENOENT, "open", "missing/server.pid")

The same with Ocsigen.Server.start: ~logdir with a syslog facility is
refused, and ~syslog_facility:None is allowed.

  $ dune build ./test.exe 2>&1
  $ ./_build/default/test.exe syslog >out.log 2>&1; echo "exit $?"
  exit 50
  $ grep -o 'Fatal - .*' out.log
  Fatal - a log directory and syslog are mutually exclusive: the logs go either to files or to syslog. Set only one of them (<logdir> or <syslog> in the configuration file, ~logdir or ~syslog_facility for Ocsigen.Server.start).
  $ ./_build/default/test.exe nosyslog >out.log 2>&1; echo "exit $?"
  exit 9
  $ grep -o 'Fatal - .*' out.log
  Fatal - Unix.Unix_error(Unix.ENOENT, "open", "missing/server.pid")
