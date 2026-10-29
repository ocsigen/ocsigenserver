A configuration file that is not well-formed XML stops the server at
start-up, with the name of the file and the position of the error.

  $ cat > bad.conf <<CONF
  > <ocsigen><server>
  > <port>0</port>
  > <host>
  > </hostx>
  > </server></ocsigen>
  > CONF
  $ ocsigenserver -c bad.conf >out.log 2>&1; echo "exit $?"
  exit 50
  $ sed 's/^.*\[ERROR\] //' out.log
  Fatal - Error in configuration file: bad.conf, line 4, characters 0-7: End of tag expected : 'host'

So does a configuration file that does not exist:

  $ ocsigenserver -c missing.conf >out.log 2>&1; echo "exit $?"
  exit 50
  $ sed 's/^.*\[ERROR\] //' out.log
  Fatal - Error in configuration file: cannot open missing.conf

And so does a file that its own DTD rejects:

  $ cat > dtd.conf <<CONF
  > <!DOCTYPE ocsigen [ <!ELEMENT ocsigen EMPTY> ]>
  > <ocsigen><server><port>0</port></server></ocsigen>
  > CONF
  $ ocsigenserver -c dtd.conf >out.log 2>&1; echo "exit $?"
  exit 50
  $ sed 's/^.*\[ERROR\] //' out.log
  Fatal - Error in configuration file: dtd.conf: No more children expected

A file of an <extconf> directory that is not well-formed XML is logged, with
its name and the position of the error, and ignored.

A user configuration file (Userconf) that is not well-formed XML gives a 500,
and the error is logged with the name of the file and the position of the
error. A user without a configuration file is skipped: the next instruction
answers.

  $ . ../server-test-helpers.sh
  $ free_ports 1
  $ mkdir -p log www conf.d
  $ echo index > www/index.html
  $ printf '<ocsigen><server>\n<host>\n</hostx>\n</server></ocsigen>\n' \
  >   > conf.d/bad.conf
  $ cat > site.conf <<CONF
  > <ocsigen><server>
  > <port>127.0.0.1:$PORT1</port>
  > <logdir>log</logdir>
  > <commandpipe>local.cmd</commandpipe>
  > <extension findlib-package="ocsigenserver.ext.staticmod"/>
  > <extension findlib-package="ocsigenserver.ext.userconf"/>
  > <host hostfilter="*">
  >   <userconf regexp="([a-z]+)/(.*)" conf="\1.conf" url="\2" prefix="\1"
  >             localpath="www"/>
  >   <static dir="www" />
  > </host>
  > <extconf dir="conf.d"/>
  > </server></ocsigen>
  > CONF
  $ printf '<site>\n<static dir="www">\n</site>\n' > user.conf
  $ ocsigenserver -c site.conf >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'stop_server $SERVER_PID' EXIT
  $ wait_server $SERVER_PID server.log local.cmd \
  >   http://127.0.0.1:$PORT1/index.html
  $ grep -A 1 'ocsigen:config: \[ERROR\]' log/errors.log | sed 's/^.*: \[ERROR\] //; s/^ *//'
  Error while loading configuration file conf.d/bad.conf (ignored)
  conf.d/bad.conf, line 3, characters 0-7: End of tag expected : 'host'
  $ curl -s -o /dev/null -w '%{http_code}\n' --max-time 10 \
  >   http://127.0.0.1:$PORT1/user/index.html
  500
  $ grep -o 'ocsigen:ext:userconf: .*' log/errors.log | sed "s/:$PORT1\//:PORT\//"
  ocsigen:ext:userconf: [ERROR] Syntax error in userconf configuration file for url //127.0.0.1:PORT/user/index.html: user.conf, line 3, characters 0-6: End of tag expected : 'static'
  $ curl -s -o /dev/null -w '%{http_code}\n' --max-time 10 \
  >   http://127.0.0.1:$PORT1/nouser/index.html
  404
