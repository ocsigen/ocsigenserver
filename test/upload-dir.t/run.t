An empty <uploaddir> forbids uploads, in <server> as in a host or a site.
Self-contained POSIX sh.

  $ dune exec ./test.exe 2>&1
  Some "/var/upload"
  None

In a host, through Extendconfiguration, the server accepts it:

  $ mkdir -p log www
  $ echo hello > www/a.txt
  $ cat > conf.xml <<EOF
  > <ocsigen><server>
  >   <port>8097</port>
  >   <logdir>log</logdir><commandpipe>local.cmd</commandpipe>
  >   <extension findlib-package="ocsigenserver.ext.staticmod"/>
  >   <extension findlib-package="ocsigenserver.ext.extendconfiguration"/>
  >   <host hostfilter="*">
  >     <uploaddir></uploaddir>
  >     <static dir="www"/>
  >   </host>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c conf.xml >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT
  $ curl -s --max-time 10 --retry 20 --retry-delay 1 --retry-connrefused \
  >   http://127.0.0.1:8097/a.txt
  hello
