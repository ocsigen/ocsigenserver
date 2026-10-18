The server raises its soft limit on open files to what its connections need,
up to the hard limit, and accepts no more connections than the limit affords:
each connection may use two files (its socket, and a static file it sends or a
connection to a proxied server), and 100 files are kept for the rest of the
server.

  $ dune build ./limits.exe ./server.exe 2>&1

Open_files raises the soft limit, never lowers it, and keeps to the hard limit:

  $ (ulimit -Sn 256; ./_build/default/limits.exe)
  soft limit: 256
  raised to 300: 300
  not lowered to 100: 300
  raised further, up to the hard limit: true
  hard limit unchanged: true
  read again: true

With at most 300 open files, the server affords (300 - 100) / 2 = 100
connections. A <maxconnected> of 1000 is lowered to that, with a warning that
says how to accept more.

  $ start () {
  >   mkdir -p $1/log $1/data
  >   sh -c "ulimit $2; exec ./_build/default/server.exe $1" \
  >     >$1/server.log 2>&1 &
  >   i=0; while [ ! -e $1.sock ] || [ ! -e $1.cmd ]; do
  >     i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done
  > }
  $ stop () { echo shutdown > $1.cmd; wait; }
  $ start explicit "-n 300"
  $ curl --unix-socket explicit.sock -s http://localhost/; echo
  100 connections, soft limit 300
  $ stop explicit
  $ grep -o "<maxconnected> is 1000, but 300 open files afford 100 connections only" explicit/log/warnings.log
  <maxconnected> is 1000, but 300 open files afford 100 connections only

Left to its default, 10000, the maximum is lowered the same way, without a
warning: the default is meant to fit the system.

  $ start default "-n 300"
  $ curl --unix-socket default.sock -s http://localhost/; echo
  100 connections, soft limit 300
  $ stop default
  $ grep -c "open files afford" default/log/warnings.log
  0
  [1]

When the hard limit allows it, the soft limit is raised to what 1000
connections need, 2 * 1000 + 100 = 2100 files, and no further: the programs
that the server runs inherit it.

  $ start fitted "-Sn 256"
  $ curl --unix-socket fitted.sock -s http://localhost/; echo
  1000 connections, soft limit 2100
  $ stop fitted

With at most 100 open files, no connection is left once the server has kept
its 100: it cannot accept any, and says so, even with the default.

  $ start tiny "-n 100"
  $ stop tiny
  $ grep -o "100 open files afford 0 connections, .*: it cannot accept any." tiny/log/errors.log
  100 open files afford 0 connections, and the server needs one for each port it listens on (1): it cannot accept any.
