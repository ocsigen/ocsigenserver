A user configuration file (Userconf) can serve the files of the user. A file
that the server cannot parse, or that sets an option refused to users, gets a
500, and the error log says which of the two it is.

  $ . ../server-test-helpers.sh
  $ free_ports 1
  $ mkdir -p log users/alice users/bob users/carol users/dave \
  >   users/erin users/frank users/ivan www/sub "www/~ivan/sub"
  $ echo hello > users/alice/a.txt
  $ echo '<static dir="."/>' > users/alice.conf
  $ echo '<followsymlinks value="sometimes"/>' > users/dave.conf
  $ echo '<followsymlinks value="always"/>' > users/erin.conf
  $ echo '<revproxy suburl="(.*)" dest="http://127.0.0.1:1/\1"/>' \
  >   > users/bob.conf
  $ echo '<if><ip value="127.0.0.1"/><then><allow-forward-for/></then></if>' \
  >   > users/carol.conf
  $ echo '<allow-forward-proto/>' > users/frank.conf
  $ echo '<listdirs value="true"/>' > users/ivan.conf
  $ echo hidden > www/sub/g.txt
  $ cat > conf.xml <<EOF
  > <ocsigen><server>
  >   <port>127.0.0.1:$PORT1</port>
  >   <logdir>log</logdir>
  >   <commandpipe>local.cmd</commandpipe>
  >   <extension findlib-package="ocsigenserver.ext.staticmod"/>
  >   <extension findlib-package="ocsigenserver.ext.extendconfiguration"/>
  >   <extension findlib-package="ocsigenserver.ext.accesscontrol"/>
  >   <extension findlib-package="ocsigenserver.ext.revproxy"/>
  >   <extension findlib-package="ocsigenserver.ext.outputfilter"/>
  >   <extension findlib-package="ocsigenserver.ext.userconf"/>
  >   <host hostfilter="*">
  >     <userconf regexp="~([^/]*)/(.*)" conf="users/\1.conf" prefix="~\1"
  >               url="\2" localpath="users/\1"/>
  >     <static dir="www"/>
  >     <if><path regexp="~alice/.*"/>
  >       <then><outputfilter header="x-alice" dest="yes"/></then></if>
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
  ocsigen:ext:userconf: [ERROR] Syntax error in userconf configuration file for url /~dave/a.txt: Wrong value "sometimes" for option "followsymlinks"
  $ get '~erin/a.txt'
  500
  $ errors
  ocsigen:ext:userconf: [ERROR] Unauthorized option in user configuration for url /~erin/a.txt: Cannot specify value 'always' for option 'followsymlinks' in userconf files

A user cannot forward requests, which would reach the services that only the
server can reach, nor make the server trust the X-Forwarded-For and
X-Forwarded-Proto headers, which any client can send:

  $ get '~bob/x'
  500
  $ errors
  ocsigen:ext:userconf: [ERROR] Unauthorized option in user configuration for url /~bob/x: <revproxy> is not allowed in user configuration files
  $ get '~carol/a.txt'
  500
  $ errors
  ocsigen:ext:userconf: [ERROR] Unauthorized option in user configuration for url /~carol/a.txt: <allow-forward-for/> is not allowed in user configuration files
  $ get '~frank/a.txt'
  500
  $ errors
  ocsigen:ext:userconf: [ERROR] Unauthorized option in user configuration for url /~frank/a.txt: <allow-forward-proto/> is not allowed in user configuration files

When a user file does not answer, the directives that follow Userconf get the
request as it came, without the settings of the user file nor the path that
Userconf gave it. Here, the static files of the main configuration are not
listed, and sub/ is not reached through ~ivan/sub/:

  $ get '~ivan/sub/'
  404
  $ get '~ivan/sub/g.txt'
  404

The same holds for a request that a user file answers: the directives that
follow Userconf see its path as it came.

  $ curl -s -D - -o /dev/null "http://127.0.0.1:$PORT1/~alice/a.txt" \
  > | grep -i '^x-alice' | tr -d '\r'
  x-alice: yes
