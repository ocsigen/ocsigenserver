A user configuration file (Userconf) can serve the files of the user. A file
that the server cannot parse, or that sets an option refused to users, gets a
500, and the error log says which of the two it is.

  $ . ../server-test-helpers.sh
  $ free_ports 1
  $ mkdir -p log users/alice users/dave users/erin
  $ echo hello > users/alice/a.txt
  $ echo '<static dir="."/>' > users/alice.conf
  $ echo '<followsymlinks value="sometimes"/>' > users/dave.conf
  $ echo '<followsymlinks value="always"/>' > users/erin.conf
  $ cat > conf.xml <<EOF
  > <ocsigen><server>
  >   <port>127.0.0.1:$PORT1</port>
  >   <logdir>log</logdir>
  >   <commandpipe>local.cmd</commandpipe>
  >   <extension findlib-package="ocsigenserver.ext.staticmod"/>
  >   <extension findlib-package="ocsigenserver.ext.extendconfiguration"/>
  >   <extension findlib-package="ocsigenserver.ext.userconf"/>
  >   <host hostfilter="*">
  >     <userconf regexp="~([^/]*)/(.*)" conf="users/\1.conf" prefix="~\1"
  >               url="\2" localpath="users/\1"/>
  >   </host>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c conf.xml >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'stop_server $SERVER_PID' EXIT
  $ wait_server $SERVER_PID server.log local.cmd \
  >   "http://127.0.0.1:$PORT1/~alice/a.txt"
  $ get () {
  >   curl -s -o /dev/null -w '%{http_code}\n' --max-time 10 \
  >     "http://127.0.0.1:$PORT1/$1"
  > }
  $ errors () {
  >   grep -o 'ocsigen:ext:userconf: .*' log/errors.log | tail -n 1
  > }

  $ get '~alice/a.txt'
  200
  $ get '~dave/a.txt'
  500
  $ errors
  ocsigen:ext:userconf: [ERROR] Syntax error in userconf configuration file for url a.txt: Wrong value "sometimes" for option "followsymlinks"
  $ get '~erin/a.txt'
  500
  $ errors
  ocsigen:ext:userconf: [ERROR] Unauthorized option in user configuration for url a.txt: Cannot specify value 'always' for option 'followsymlinks' in userconf files
