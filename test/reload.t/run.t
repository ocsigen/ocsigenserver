The reload order of the command pipe reads the configuration file once, and
applies its changes. Self-contained (POSIX sh).

  $ . ../server-test-helpers.sh
  $ mkdir -p log www new
  $ echo index > www/index.html
  $ echo new > new/index.html

The configuration has a tag that logs a warning each time it is read, and
the sites given as argument:

  $ write_conf () {
  >   cat > site.conf <<CONF
  > <ocsigen><server>
  > <port>8091</port>
  > <logdir>log</logdir>
  > <commandpipe>local.cmd</commandpipe>
  > <servertimeout>30</servertimeout>
  > <extension findlib-package="ocsigenserver.ext.staticmod"/>
  > <host hostfilter="*">
  > $1
  >   <static dir="www" />
  > </host>
  > </server></ocsigen>
  > CONF
  > }
  $ count_warnings () {
  >   grep -c '<servertimeout> has no effect' log/warnings.log
  > }
  $ reload () {
  >   send_command $SERVER_PID local.cmd reload
  >   i=0; until [ "$(grep -c "$1" log/warnings.log)" -ge "$2" ]; do
  >     i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done
  > }

  $ write_conf ''
  $ ocsigenserver -c site.conf >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT
  $ wait_server $SERVER_PID server.log local.cmd \
  >   http://127.0.0.1:8091/index.html
  $ count_warnings
  1
  $ curl -s -o /dev/null -w '%{http_code}\n' --max-time 10 \
  >   http://127.0.0.1:8091/new/index.html
  404

A reload reads the file once: the warning appears once more. The new site
is served.

  $ write_conf '<site path="new"><static dir="new" /></site>'
  $ reload 'Config file reloaded' 1
  $ count_warnings
  2
  $ curl -s --max-time 10 http://127.0.0.1:8091/new/index.html
  new

A file with an error is reported once, and the server keeps its sites, here
the one that the file no longer has:

  $ write_conf '<nosuchtag />'
  $ reload 'Config file reload failed' 1
  $ grep -o 'ocsigen:main: .*' log/errors.log
  ocsigen:main: [ERROR] Fatal - Error in configuration file: Unexpected tag <nosuchtag> inside <site dir="">
  $ curl -s --max-time 10 http://127.0.0.1:8091/new/index.html
  new

  $ (sleep 10; kill $SERVER_PID) >/dev/null 2>&1 &
  $ WATCHDOG=$!
  $ send_command $SERVER_PID local.cmd shutdown
  $ wait $SERVER_PID; echo "exit $?"
  exit 0
  $ kill $WATCHDOG
