Rewritemod rewrites the path of the requests that match its regexp, then
the request starts again from the beginning. POSIX sh.

  $ . ../server-test-helpers.sh
  $ mkdir -p log www/new
  $ echo hello > www/new/a.txt
  $ ../rewritemod/test.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ wait_server $SERVER_PID server.log local.sock local.cmd
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

  $ stop_server $SERVER_PID
