Errors that the client only sees as a generic status are logged above the
debug level, so that the server's logs say what went wrong.

  $ mkdir www
  $ printf 'secret' > www/locked.txt
  $ chmod 000 www/locked.txt
  $ cat www/locked.txt >/dev/null 2>&1 && echo "running as root: the test is meaningless"
  [1]
  $ ocsigenserver --serve www --port 8077 >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT

A file that the server cannot read gives a 403, and a warning:

  $ curl -s -o /dev/null -w '%{http_code}\n' --max-time 10 --retry 20 \
  >   --retry-delay 1 --retry-connrefused http://127.0.0.1:8077/locked.txt
  403
  $ grep -c 'Permission denied while handling request' server.log
  1
