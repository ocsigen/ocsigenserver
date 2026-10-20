Outputfilter.run replaces every match of its regexp in the value of the
header, as <outputfilter> does in the configuration file. POSIX sh.

  $ . ../server-test-helpers.sh
  $ mkdir -p log www
  $ echo hello > www/a.txt
  $ dune build ./test.exe 2>&1
  $ ./_build/default/test.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ wait_server $SERVER_PID server.log local.sock local.cmd
  $ curl --unix-socket local.sock -s -D - -o /dev/null http://x/a.txt \
  > | grep -i '^content-type' | tr -d '\r'
  content-type: text/html
  $ stop_server $SERVER_PID
