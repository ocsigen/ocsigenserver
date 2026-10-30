Errors that the client only sees as a generic status are logged above the
debug level, so that the server's logs say what went wrong.

  $ . ../server-test-helpers.sh
  $ mkdir -p log www
  $ printf 'secret' > www/locked.txt
  $ chmod 000 www/locked.txt
  $ cat www/locked.txt >/dev/null 2>&1 && echo "running as root: the 403 is not tested"
  [1]
  $ ../error-logs/test.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'stop_server $SERVER_PID' EXIT
  $ wait_server $SERVER_PID server.log local.sock local.cmd
  $ get () { curl --unix-socket local.sock -s -w '\n%{http_code}\n' "http://x/$1"; }

A file that the server cannot read gives a 403, and a warning:

  $ get locked.txt
  Error: Forbidden
  403
  $ grep -c 'Permission denied while handling request' log/warnings.log
  1

An exception in an extension gives a 500, and an error that names it. The
client does not see the exception:

  $ get fail
  Error: Internal Server Error
  500
  $ grep -A1 'Error while handling request' log/errors.log | sed 's/^.*\[ERROR\] //; s/^ *//'
  Error while handling request.
  Failure("test failure")
