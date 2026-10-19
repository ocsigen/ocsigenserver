The Content-Type of static files comes from the <contenttype> rules, then
the <mimefile>, then the built-in table of magic-mime. Text and XML types
get the charset of the <charset> rules. Self-contained POSIX sh.

  $ mkdir -p log data www
  $ for f in a.txt b.html c.zzz bla.qqq d.png e.unknownext f.svg; do
  >   echo x > www/$f; done
  $ printf 'text/x-zzz zzz\n' > my.types
  $ cat > conf.xml <<EOF
  > <ocsigen><server>
  >   <port>127.0.0.1:8113</port>
  >   <logdir>log</logdir><datadir>data</datadir>
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
  $ ocsigenserver -c conf.xml >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT
  $ curl -s -o /dev/null --max-time 10 --retry 20 --retry-delay 1 \
  >   --retry-connrefused http://127.0.0.1:8113/a.txt
  $ for f in a.txt b.html c.zzz bla.qqq d.png e.unknownext f.svg; do
  >   printf '%s: ' $f
  >   curl -s -D - -o /dev/null http://127.0.0.1:8113/$f \
  >   | grep -i '^content-type' | tr -d '\r'
  > done
  a.txt: content-type: text/plain; charset=iso-8859-15
  b.html: content-type: text/html; charset=utf-8
  c.zzz: content-type: text/x-zzz; charset=utf-8
  bla.qqq: content-type: text/x-foo; charset=utf-8
  d.png: content-type: image/png
  e.unknownext: content-type: application/octet-stream
  f.svg: content-type: image/svg+xml; charset=utf-8
