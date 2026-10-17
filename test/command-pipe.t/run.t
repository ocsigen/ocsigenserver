The command pipe is a control channel (shutdown, reload <file>, ...): only the
user running the server may write to it. An existing file at the pipe path is
checked before it is used. A path that belongs to another user is not tested
here: it needs a second user. Self-contained (POSIX sh) so it runs regardless
of the cram shell.

  $ mkdir -p log data
  $ dune build ./test.exe 2>&1
  $ start_server () {
  >   rm -f local.sock log/*
  >   ./_build/default/test.exe >server.log 2>&1 &
  >   SERVER_PID=$!
  >   i=0; while [ ! -e local.sock ]; do
  >     i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done
  > }
  $ wait_for_log () {
  >   i=0; until grep -q "$1" log/warnings.log 2>/dev/null; do
  >     i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done
  > }
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT

A FIFO with group permissions, as Ocsigen Server 7 created it, is replaced with
a new one: a process that opened it before would keep its descriptor after a
mere change of mode. Here the shell holds such a descriptor, and writes orders
to it before and after the server starts.

  $ mkfifo -m 660 local.cmd
  $ ls -i local.cmd >inode
  $ exec 3<>local.cmd
  $ echo shutdown >&3
  $ start_server
  $ echo shutdown >&3
  $ ls -l local.cmd | cut -c 1-10
  prw-------
  $ ls -i local.cmd | cmp -s - inode || echo replaced
  replaced
  $ grep -o 'Command pipe local.cmd has group.*' log/warnings.log
  Command pipe local.cmd has group or other permissions: replacing it

Only the orders written to the new pipe reach the server.

  $ echo hello >local.cmd
  $ wait_for_log 'Command received: hello'
  $ grep -o 'Command received: .*' log/warnings.log
  Command received: hello
  $ exec 3>&-
  $ kill $SERVER_PID; wait

Anything else than a named pipe is refused, so that another user cannot plant a
file of commands there. Here, a regular file containing "shutdown": the server
does not read it, and keeps serving without a command pipe.

  $ rm -f local.cmd
  $ echo shutdown >local.cmd
  $ start_server
  $ grep -o 'Refusing the command pipe.*' log/errors.log
  Refusing the command pipe local.cmd: it is not a named pipe. Remove it to let the server create a new one. I will continue without.
  $ curl --unix-socket local.sock -s -o /dev/null -w '%{http_code}\n' http://x/
  404
  $ kill $SERVER_PID; wait
