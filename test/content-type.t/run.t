The Content-Type of static files comes from the <contenttype> rules, then
the <mimefile>, then the built-in table of magic-mime. Text and XML types
get the charset of the <charset> rules.

  $ . ../server-test-helpers.sh
  $ free_ports 2
  $ trap 'kill $CONF_PID $SERVE_PID 2>/dev/null; stop_server $LIB_PID' EXIT
  $ mkdir -p log www
  $ for f in a.txt b.html c.zzz bla.qqq d.png e.unknownext f.svg g.js; do
  >   echo x > www/$f; done
  $ printf 'text/x-zzz zzz\n' > my.types
  $ ctypes () {
  >   ctypes_opts=$1 ctypes_base=$2; shift 2
  >   for f in "$@"; do
  >     printf '%s: ' $f
  >     curl -s $ctypes_opts -D - -o /dev/null --max-time 10 "$ctypes_base/$f" \
  >     | grep -i '^content-type' | tr -d '\r'
  >   done
  > }

With a configuration file:

  $ cat > conf.xml <<EOF
  > <ocsigen><server>
  >   <port>127.0.0.1:$PORT1</port>
  >   <logdir>log</logdir>
  >   <mimefile>my.types</mimefile>
  >   <extension findlib-package="ocsigenserver.ext.staticmod"/>
  >   <extension findlib-package="ocsigenserver.ext.extendconfiguration"/>
  >   <host hostfilter="*">
  >     <charset default="utf-8"><extension ext="txt" value="iso-8859-15"/></charset>
  >     <contenttype><file file="bla.qqq" value="text/x-foo"/></contenttype>
  >     <static dir="www"/>
  >   </host>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c conf.xml >conf.log 2>&1 &
  $ CONF_PID=$!
  $ wait_server $CONF_PID conf.log http://127.0.0.1:$PORT1/
  $ ctypes "" http://127.0.0.1:$PORT1 \
  >   a.txt b.html c.zzz bla.qqq d.png e.unknownext f.svg g.js
  a.txt: content-type: text/plain; charset=iso-8859-15
  b.html: content-type: text/html; charset=utf-8
  c.zzz: content-type: text/x-zzz; charset=utf-8
  bla.qqq: content-type: text/x-foo; charset=utf-8
  d.png: content-type: image/png
  e.unknownext: content-type: application/octet-stream
  f.svg: content-type: image/svg+xml; charset=utf-8
  g.js: content-type: application/javascript
  $ kill $CONF_PID

Without <mimefile>, no mime.types file is read: serve mode logs no error
about it, and the types come from magic-mime. Text gets the default charset,
utf-8:

  $ ocsigenserver --serve www --port $PORT2 >serve.log 2>&1 &
  $ SERVE_PID=$!
  $ wait_server $SERVE_PID serve.log http://127.0.0.1:$PORT2/
  $ ctypes "" http://127.0.0.1:$PORT2 a.txt c.zzz g.js
  a.txt: content-type: text/plain; charset=utf-8
  c.zzz: content-type: application/octet-stream
  g.js: content-type: application/javascript
  $ grep -i "mime.types\|mime types" serve.log
  [1]
  $ kill $SERVE_PID

In a program, ~mimefile of Ocsigen.Server.start applies to the hosts given
to it, which are evaluated before it:

  $ ../content-type/test.exe >lib.log 2>&1 &
  $ LIB_PID=$!
  $ wait_server $LIB_PID lib.log local.sock local.cmd
  $ ctypes "--unix-socket local.sock" http://x c.zzz d.png
  c.zzz: content-type: text/x-zzz; charset=utf-8
  d.png: content-type: image/png

With ~default_charset:None, the server has no default charset:

  $ stop_server $LIB_PID
  $ rm local.sock
  $ ../content-type/test.exe --no-charset >lib.log 2>&1 &
  $ LIB_PID=$!
  $ wait_server $LIB_PID lib.log local.sock local.cmd
  $ ctypes "--unix-socket local.sock" http://x c.zzz
  c.zzz: content-type: text/x-zzz
