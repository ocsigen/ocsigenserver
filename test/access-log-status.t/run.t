A response with a status code that Cohttp has no constructor for is sent and
logged like any other.

  $ mkdir -p log data
  $ dune build ./test.exe 2>&1
  $ ./_build/default/test.exe >server.log 2>&1 &
  $ trap 'echo shutdown > local.cmd 2>/dev/null; wait' EXIT
  $ i=0; while [ ! -e local.sock ] || [ ! -e local.cmd ]; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done

  $ curl --unix-socket local.sock -s -o /dev/null -w '%{http_code}\n' \
  >   --user-agent "test-agent" http://x/page
  421
  $ for _ in $(seq 1 20); do
  >   grep -q "GET /page" log/access.log && break
  >   sleep 0.5
  > done
  $ sed 's/\[[^]]*\]/[TIME]/' log/access.log
  - - - [TIME] "GET /page HTTP/1.1" 421 1 "-" "test-agent"
