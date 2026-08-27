Security hardening checks: command pipe permissions (F8) and error-body
sanitisation (F4). Self-contained (POSIX sh, no bash helper) so it runs
regardless of the cram shell.

  $ mkdir -p log data
  $ dune build ./test.exe 2>&1
  $ dune exec -- ./test.exe >server.log 2>&1 &
  $ trap 'echo shutdown > local.cmd 2>/dev/null; wait' EXIT
  $ i=0; while [ ! -e local.sock ] || [ ! -e local.cmd ]; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done

F8: the command pipe is created with mode 0o600, so that only the server's own
user can send it control commands.

  $ stat -c '%a' local.cmd
  600

F4: a handler error returns the generic HTTP status reason phrase, never the
OCaml exception (the word "secret" from the exception must not appear).

  $ curl --unix-socket local.sock -s -o /dev/null -w '%{http_code}\n' http://x/anything
  500
  $ curl --unix-socket local.sock -s http://x/anything
  Error: Internal Server Error
  $ curl --unix-socket local.sock -s http://x/anything | grep -c secret || true
  0
