A response with a status code that Cohttp has no constructor for is sent and
logged like any other.

  $ . ../server-test-helpers.sh
  $ mkdir -p log
  $ ../access-log-status/test.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'stop_server $SERVER_PID' EXIT
  $ wait_server $SERVER_PID server.log local.sock local.cmd

  $ curl --unix-socket local.sock -s -o /dev/null -w '%{http_code}\n' \
  >   --user-agent "test-agent" http://x/page
  421
  $ for _ in $(seq 1 20); do
  >   grep -q "GET /page" log/access.log && break
  >   sleep 0.5
  > done
  $ sed 's/\[[^]]*\]/[TIME]/' log/access.log
  - - - [TIME] "GET /page HTTP/1.1" 421 1 "-" "test-agent"
