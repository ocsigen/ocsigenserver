Rewritemod rewrites the path of the requests that match its regexp, then
the request starts again from the beginning. Self-contained POSIX sh.

  $ mkdir -p log data www/new
  $ echo hello > www/new/a.txt
  $ dune build ./test.exe 2>&1
  $ ./_build/default/test.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ i=0; while [ ! -e local.sock ] || [ ! -e local.cmd ]; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done
  $ get () { curl --unix-socket local.sock -s "http://x/$1"; }
  $ status () {
  >   curl --unix-socket local.sock -s -o /dev/null -w '%{http_code}\n' \
  >     "http://x/$1"
  > }

A request that does not match the regexp goes on to the next instruction:

  $ get new/a.txt
  hello

A request that matches is rewritten. Once rewritten, it does not match any
more, and Staticmod serves it:

  $ get old/a.txt
  hello
  $ status old/missing
  404

  $ echo shutdown > local.cmd
  $ wait $SERVER_PID
