The files uploaded in a multipart request are removed once the request is
handled. Self-contained POSIX sh.

  $ mkdir -p log uploads kept
  $ echo hello > a.txt
  $ ../upload-cleanup/test.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ i=0; while [ ! -e local.sock ] || [ ! -e local.cmd ]; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done
  $ post () { curl --unix-socket local.sock -s "$@" http://x/; }

The file exists while the request is handled, and is gone after:

  $ post -F f=@a.txt -F g=@a.txt
  f a.txt true
  g a.txt true
  $ ls uploads

A file that the application moved is kept, without a warning:

  $ post -F keep=@a.txt
  keep a.txt false
  $ ls uploads kept
  kept:
  a.txt
  
  uploads:
  $ grep -c 'Error while removing' log/warnings.log
  0
  [1]

The files created after the answer, when the application does not wait for
them, are removed once the body is decoded. The big file comes first, so that
the second one is created after the answer:

  $ dd if=/dev/zero of=big bs=1024 count=8192 2>/dev/null
  $ curl --unix-socket local.sock -s -F big=@big -F f=@a.txt http://x/early
  early
  $ i=0; while [ -n "$(ls uploads)" ]; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done
  $ ls uploads

  $ echo shutdown > local.cmd
  $ wait $SERVER_PID
