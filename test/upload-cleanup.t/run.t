The files uploaded in a multipart request are removed once the request is
handled. POSIX sh.

  $ . ../server-test-helpers.sh
  $ mkdir -p log uploads kept
  $ echo hello > a.txt
  $ ../upload-cleanup/test.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ wait_server $SERVER_PID server.log local.sock local.cmd
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

A part without a Content-Disposition header, or without a name in it, is a
bad request. The file of a previous part is removed all the same. raw sends
its first argument as the body, with the Content-Type given as second
argument, if any, and prints the status:

  $ raw () {
  >   printf %b "$1" | curl --unix-socket local.sock -s -o /dev/null \
  >     -w '%{http_code}\n' --data-binary @- \
  >     -H "Content-Type: ${2:-multipart/form-data; boundary=B}" http://x/; }
  $ raw '--B\r\nContent-Type: text/plain\r\n\r\nx\r\n--B--\r\n'
  400
  $ raw '--B\r\nContent-Disposition: form-data\r\n\r\nx\r\n--B--\r\n'
  400
  $ raw '--B\r\nContent-Disposition: form-data; name="f"; filename="a.txt"\r\n\r\nx\r\n--B\r\n\r\ny\r\n--B--\r\n'
  400
  $ ls uploads

A malformed body is a bad request too: a part header that is not a header,
a boundary without line end, a multipart Content-Type without boundary. The
file of a previous part is removed all the same:

  $ raw '--B\r\nContent-Disposition: form-data; name="f"; filename="a.txt"\r\n\r\nx\r\n--B\r\nfoo\r\n\r\ny\r\n--B--\r\n'
  400
  $ raw '--B'
  400
  $ raw '--B\r\nContent-Disposition: form-data; name="f"\r\n\r\nx\r\n--B--\r\n' multipart/form-data
  400
  $ ls uploads

A boundary has 1 to 70 characters (RFC 2046), so an empty boundary and one
of 71 characters are refused. One of 70 characters is accepted. An empty
value without quotes makes the parameters malformed, so that the
Content-Type has no boundary:

  $ raw '--\r\nContent-Disposition: form-data; name="f"\r\n\r\nx\r\n----\r\n' 'multipart/form-data; boundary=""'
  400
  $ raw '--\r\nContent-Disposition: form-data; name="f"\r\n\r\nx\r\n----\r\n' 'multipart/form-data; boundary='
  400
  $ b70=$(printf %070d 0)
  $ for b in ${b70}1 $b70; do
  >   raw "--$b\r\nContent-Disposition: form-data; name=\"f\"\r\n\r\nx\r\n--$b--\r\n" \
  >     "multipart/form-data; boundary=$b"; done
  400
  200

The name of a part is its name parameter, never its filename parameter, so a
part with only a filename is a bad request, as is a Content-Disposition with
an unterminated quote. The parameters may come in any order, with spaces
around "=" and ";", in any case, quoted or not, and with a trailing ";":

  $ raw '--B\r\nContent-Disposition: form-data; filename="a.txt"\r\n\r\nx\r\n--B--\r\n'
  400
  $ raw '--B\r\nContent-Disposition: form-data; name="f\r\n\r\nx\r\n--B--\r\n'
  400
  $ fields () {
  >   printf %b "$1" | curl --unix-socket local.sock -s --data-binary @- \
  >     -H 'Content-Type: multipart/form-data; boundary=B' http://x/; }
  $ fields '--B\r\nContent-Disposition: form-data; filename="a.txt"; name="f";\r\n\r\nx\r\n--B--\r\n'
  f a.txt true
  $ fields '--B\r\nContent-Disposition: form-data ; FileName = "name=g;" ; Name = f\r\n\r\nx\r\n--B--\r\n'
  f name=g; true
  $ fields '--B\r\nContent-Disposition: form-data; name=field; filename=a.txt\r\n\r\nx\r\n--B--\r\n'
  field a.txt true

In a quoted value, \" and \\ are escapes, as some clients send them; another
backslash is kept (printf %b turns each \\ below into one backslash):

  $ fields '--B\r\nContent-Disposition: form-data; name="f"; filename="a\\"b\\\\c\\d.txt"\r\n\r\nx\r\n--B--\r\n'
  f a"b\c\d.txt true

Browsers do not escape backslashes, so a name or a file name that ends with
one comes with a backslash before its closing quote. Read with escapes, the
parameters are malformed, so they are read again with every backslash kept:

  $ fields '--B\r\nContent-Disposition: form-data; name="f"; filename="dir\\"\r\n\r\nx\r\n--B--\r\n'
  f dir\ true
  $ fields '--B\r\nContent-Disposition: form-data; name="f\\"; filename="a.txt"\r\n\r\nx\r\n--B--\r\n'
  f\ a.txt true
  $ ls uploads

The reasons are logged:

  $ sed -n 's/.*Multipart body rejected: //p' server.log
  a part has no Content-Disposition header
  a part has no name in its Content-Disposition
  a part has no Content-Disposition header
  a part has a malformed header
  the body or one of its parts ends too early
  its Content-Type has no boundary
  its boundary is empty
  its Content-Type has no boundary
  its boundary is longer than 70 characters
  a part has no name in its Content-Disposition
  a part has a malformed Content-Disposition

  $ stop_server $SERVER_PID
