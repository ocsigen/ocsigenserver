Security hardening checks: command pipe permissions (F8) and error-body
sanitisation (F4). POSIX sh, so that it runs regardless of the cram shell.

  $ . ../server-test-helpers.sh
  $ mkdir -p log
  $ ../security/test.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'stop_server $SERVER_PID' EXIT
  $ wait_server $SERVER_PID server.log local.sock local.cmd

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
