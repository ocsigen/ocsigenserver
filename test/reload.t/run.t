The reload order of the command pipe reads the configuration file once, and
applies its changes. Self-contained (POSIX sh).

  $ . ../server-test-helpers.sh
  $ mkdir -p log www/dir new
  $ echo index > www/index.html
  $ echo dir > www/dir/index.html
  $ echo new > new/index.html

The configuration has a tag that logs a warning each time it is read, the
sites given as first argument, and the tags of <server> given as second
argument:

  $ write_conf () {
  >   cat > site.conf <<CONF
  > <ocsigen><server>
  > <port>8091</port>
  > <logdir>log</logdir>
  > <commandpipe>local.cmd</commandpipe>
  > <servertimeout>30</servertimeout>
  > <extension findlib-package="ocsigenserver.ext.staticmod"/>
  > $2
  > <host hostfilter="*" defaulthostname="example.org">
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
  >   i=0; until [ "$(cat log/*.log | grep -c "$1")" -ge "$2" ]; do
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

A file with an error is reported once, in a single error line, and the server
keeps its sites, here the one that the file no longer has:

  $ write_conf '<nosuchtag />'
  $ reload 'Config file reload failed' 1
  $ grep -o 'ocsigen:main: .*' log/errors.log
  ocsigen:main: [ERROR] Config file reload failed: Error in configuration file: Unexpected tag <nosuchtag> inside <site dir="">
  $ grep -c 'reload failed' log/warnings.log
  0
  [1]
  $ curl -s --max-time 10 http://127.0.0.1:8091/new/index.html
  new

So is a file that is not well-formed XML, with the name of the file and the
position of the error:

  $ write_conf '<site path="new"><static dir="new" /></sitex>'
  $ reload 'Config file reload failed' 2
  $ grep -o 'ocsigen:main: .*' log/errors.log | tail -n 1
  ocsigen:main: [ERROR] Config file reload failed: Error in configuration file: site.conf, line 9, characters 37-44: End of tag expected : 'site'
  $ curl -s --max-time 10 http://127.0.0.1:8091/new/index.html
  new

A failed reload does not apply the settings of <server> that come before the
error either. With <usedefaulthostname/>, the redirection to a directory uses
the host name of the configuration rather than the one of the request:

  $ redirection () {
  >   curl -s -o /dev/null -w '%{redirect_url}\n' --max-time 10 \
  >     http://127.0.0.1:8091/dir
  > }
  $ redirection
  http://127.0.0.1:8091/dir/
  $ write_conf '<nosuchtag />' '<usedefaulthostname/>'
  $ reload 'Config file reload failed' 3
  $ redirection
  http://127.0.0.1:8091/dir/
  $ write_conf '' '<usedefaulthostname/>'
  $ reload 'Config file reloaded' 2
  $ redirection
  http://example.org:8091/dir/

  $ (sleep 10; kill $SERVER_PID) >/dev/null 2>&1 &
  $ WATCHDOG=$!
  $ send_command $SERVER_PID local.cmd shutdown
  $ wait $SERVER_PID; echo "exit $?"
  exit 0
  $ kill $WATCHDOG
