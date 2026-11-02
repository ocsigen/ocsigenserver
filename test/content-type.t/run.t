The Content-Type of static files comes from the <contenttype> rules, then
the <mimefile>, then the built-in table of magic-mime, then the default of
<contenttype>. Text and XML types get the charset of the <charset> rules.

  $ . ../server-test-helpers.sh
  $ free_ports 2
  $ trap 'kill $CONF_PID $SERVE_PID 2>/dev/null; stop_server $LIB_PID' EXIT
  $ mkdir -p log www
  $ for f in a.txt b.html c.zzz bla.qqq d.png e.unknownext f.svg g.js h.yyy \
  >   i.csv j.lat k.feed; do
  >   echo x > www/$f; done
  $ echo x > www/README

The mime.types file gives a type to an extension that magic-mime knows (js),
and to one that a rule gives too (yyy):

  $ printf 'text/x-zzz zzz\ntext/x-custom-js js\ntext/x-yyy yyy\n' > my.types
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
  >   <commandpipe>conf.cmd</commandpipe>
  >   <mimefile>my.types</mimefile>
  >   <extension findlib-package="ocsigenserver.ext.staticmod"/>
  >   <extension findlib-package="ocsigenserver.ext.extendconfiguration"/>
  >   <host hostfilter="*">
  >     <charset default="utf-8">
  >       <extension ext="txt" value="iso-8859-15"/>
  >       <extension ext="csv" value=""/>
  >     </charset>
  >     <contenttype default="text/x-default">
  >       <file file="bla.qqq" value="text/x-foo"/>
  >       <extension ext="yyy" value="text/x-rule"/>
  >       <extension ext="lat" value="text/plain; Charset=iso-8859-1"/>
  >       <extension ext="feed" value="Application/Atom+XML; type=feed"/>
  >     </contenttype>
  >     <site path="legacy" charset=""><static dir="www"/></site>
  >     <static dir="www"/>
  >   </host>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c conf.xml >conf.log 2>&1 &
  $ CONF_PID=$!
  $ wait_server $CONF_PID conf.log http://127.0.0.1:$PORT1/
  $ ctypes "" http://127.0.0.1:$PORT1 \
  >   a.txt b.html c.zzz bla.qqq d.png e.unknownext f.svg g.js h.yyy
  a.txt: content-type: text/plain; charset=iso-8859-15
  b.html: content-type: text/html; charset=utf-8
  c.zzz: content-type: text/x-zzz; charset=utf-8
  bla.qqq: content-type: text/x-foo; charset=utf-8
  d.png: content-type: image/png
  e.unknownext: content-type: text/x-default; charset=utf-8
  f.svg: content-type: image/svg+xml; charset=utf-8
  g.js: content-type: text/x-custom-js; charset=utf-8
  h.yyy: content-type: text/x-rule; charset=utf-8

An empty charset, in a rule or on a <site> (or <host>), sends no charset:

  $ ctypes "" http://127.0.0.1:$PORT1 i.csv legacy/b.html
  i.csv: content-type: text/csv
  legacy/b.html: content-type: text/html

A type that has a charset already gets no other one:

  $ ctypes "" http://127.0.0.1:$PORT1 j.lat
  j.lat: content-type: text/plain; Charset=iso-8859-1

Types are case-insensitive, and a rule can give parameters:

  $ ctypes "" http://127.0.0.1:$PORT1 k.feed
  k.feed: content-type: Application/Atom+XML; type=feed; charset=utf-8

A reload reads the mime.types file again:

  $ reload () {
  >   send_command $CONF_PID conf.cmd reload
  >   i=0; until [ "$(grep -c 'Config file reloaded' log/warnings.log)" -ge $1 ]
  >   do i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done
  > }
  $ printf 'text/x-zzz2 zzz\n' > my.types
  $ reload 1
  $ ctypes "" http://127.0.0.1:$PORT1 c.zzz
  c.zzz: content-type: text/x-zzz2; charset=utf-8

A mime.types file that cannot be read is reported when the configuration is
read, before any request, and the types then come from magic-mime:

  $ sed 's/my\.types/missing.types/' conf.xml > conf.new && mv conf.new conf.xml
  $ reload 2
  $ grep -o 'ocsigen:mimetype: .*' log/errors.log
  ocsigen:mimetype: [ERROR] Unable to read the mime.types file: missing.types: No such file or directory
  $ ctypes "" http://127.0.0.1:$PORT1 c.zzz g.js
  c.zzz: content-type: text/x-default; charset=utf-8
  g.js: content-type: application/javascript
  $ kill $CONF_PID

Without <mimefile>, no mime.types file is read: serve mode logs no error
about it, and the types come from magic-mime, without a charset. Magic-mime
also knows some file names, such as README:

  $ ocsigenserver --serve www --port $PORT2 >serve.log 2>&1 &
  $ SERVE_PID=$!
  $ wait_server $SERVE_PID serve.log http://127.0.0.1:$PORT2/
  $ ctypes "" http://127.0.0.1:$PORT2 a.txt c.zzz g.js README
  a.txt: content-type: text/plain
  c.zzz: content-type: application/octet-stream
  g.js: content-type: application/javascript
  README: content-type: text/x-plain
  $ grep -i "mime.types\|mime types" serve.log
  [1]
  $ kill $SERVE_PID

In a program, ~mimefile of Ocsigen.Server.start applies to the hosts given
to it, which are evaluated before it. The file is read at start-up, before
the first request: removing it then changes nothing. The program answers
the other paths with www/README, through Ocsigen.Response.respond_file
without ~content_type, which takes the type from magic-mime:

  $ printf 'text/x-zzz zzz\n' > my.types
  $ ../content-type/test.exe >lib.log 2>&1 &
  $ LIB_PID=$!
  $ wait_server $LIB_PID lib.log local.sock local.cmd
  $ rm my.types
  $ ctypes "--unix-socket local.sock" http://x c.zzz d.png other
  c.zzz: content-type: text/x-zzz
  d.png: content-type: image/png
  other: content-type: text/x-plain
