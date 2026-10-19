When Revproxy gets no response from its target, the client gets a 502, and
the error log says why, without the path of the request. Self-contained
POSIX sh.

  $ mkdir -p log data
  $ cat > conf.xml <<EOF
  > <ocsigen><server>
  >   <port>127.0.0.1:8114</port>
  >   <logdir>log</logdir><datadir>data</datadir>
  >   <extension findlib-package="ocsigenserver.ext.revproxy"/>
  >   <host hostfilter="*">
  >     <revproxy suburl="(.*)" dest="http://127.0.0.1:1/\1"/>
  >   </host>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c conf.xml >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT
  $ i=0; until curl -s -o /dev/null --max-time 1 http://127.0.0.1:8114/; do
  >   i=$((i+1)); [ $i -gt 100 ] && break; sleep 0.1; done
  $ curl -s -o /dev/null -w '%{http_code}\n' 'http://127.0.0.1:8114/a?token=x'
  502
  $ grep -o 'Cannot get a response.*' log/errors.log | sort -u
  Cannot get a response from http://127.0.0.1:1: Unix.Unix_error(Unix.ECONNREFUSED, "connect", "")
