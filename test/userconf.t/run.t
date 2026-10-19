User configuration files cannot use the directives that would give a user
the network access or the trust of the server. Self-contained POSIX sh.

  $ mkdir -p log data users/alice users/bob users/carol
  $ echo hello > users/alice/a.txt
  $ echo '<static dir="."/>' > users/alice.conf
  $ echo '<revproxy suburl="(.*)" dest="http://127.0.0.1:1/\1"/>' \
  >   > users/bob.conf
  $ echo '<if><ip value="127.0.0.1"/><then><allow-forward-for/></then></if>' \
  >   > users/carol.conf
  $ cat > conf.xml <<EOF
  > <ocsigen><server>
  >   <port>127.0.0.1:8099</port>
  >   <logdir>log</logdir><datadir>data</datadir>
  >   <extension findlib-package="ocsigenserver.ext.staticmod"/>
  >   <extension findlib-package="ocsigenserver.ext.accesscontrol"/>
  >   <extension findlib-package="ocsigenserver.ext.revproxy"/>
  >   <extension findlib-package="ocsigenserver.ext.userconf"/>
  >   <host hostfilter="*">
  >     <userconf regexp="~([^/]*)/(.*)" conf="users/\1.conf" prefix="~\1"
  >               url="\2" localpath="users/\1"/>
  >   </host>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c conf.xml >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT
  $ curl -s -o /dev/null --max-time 10 --retry 20 --retry-delay 1 \
  >   --retry-connrefused http://127.0.0.1:8099/~alice/a.txt
  $ get () {
  >   curl -s -o /dev/null -w '%{http_code}\n' "http://127.0.0.1:8099/$1"
  > }

A user file can serve the files of the user:

  $ get '~alice/a.txt'
  200

But not forward requests, nor trust X-Forwarded-For:

  $ get '~bob/x'
  500
  $ get '~carol/a.txt'
  500
  $ grep -h userconf log/errors.log server.log | cut -c 18- | sort -u
  ocsigen:ext:userconf: [ERROR] Unauthorized option in user configuration for url a.txt: <allow-forward-for/> is not allowed in user configuration files
  ocsigen:ext:userconf: [ERROR] Unauthorized option in user configuration for url x: <revproxy> is not allowed in user configuration files
