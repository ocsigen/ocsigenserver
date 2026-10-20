Outputfilter.run replaces every match of its regexp in the value of the
header, as <outputfilter> does in the configuration file. Self-contained
POSIX sh.

  $ mkdir -p log www
  $ echo hello > www/a.txt
  $ dune build ./test.exe 2>&1
  $ ./_build/default/test.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ i=0; while [ ! -e local.sock ] || [ ! -e local.cmd ]; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done
  $ curl --unix-socket local.sock -s -D - -o /dev/null http://x/a.txt \
  > | grep -i '^content-type' | tr -d '\r'
  content-type: text/html
  $ echo shutdown > local.cmd
  $ wait $SERVER_PID
