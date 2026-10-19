<allow-forward-for/> takes the client address from X-Forwarded-For. An <ip>
condition evaluated afterwards tests and logs the address the server retained.

  $ . ../server-test-helpers.sh
  $ free_ports 3
  $ mkdir -p www log
  $ printf 'hello\n' > www/index.html
  $ cat > site.conf <<EOF
  > <ocsigen><server>
  > <port>127.0.0.1:$PORT1</port>
  > <port>127.0.0.1:$PORT2</port>
  > <logdir>log</logdir>
  > <loglevel source="ocsigen:ext:access-control" level="info"/>
  > <extension findlib-package="ocsigenserver.ext.staticmod"/>
  > <extension findlib-package="ocsigenserver.ext.accesscontrol"/>
  > <host hostfilter="*">
  >   <if><port value="$PORT1"/>
  >     <then><allow-forward-for/></then>
  >     <else><allow-forward-for proxies="198.51.100.0/24  2001:db8::1"/></else>
  >   </if>
  >   <if><ip value="10.0.0.0/8"/><then><forbidden/></then></if>
  >   <static dir="www" />
  > </host>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c site.conf >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT
  $ wait_server $SERVER_PID server.log http://127.0.0.1:$PORT1/index.html \
  >   http://127.0.0.1:$PORT2/index.html
  $ get () {
  >   seen=$(grep -c 'IP: ' server.log)
  >   curl -s -o /dev/null -w '%{http_code}\n' "$@"
  >   grep -o 'IP: [^ ]*' server.log | tail -n +$((seen + 1))
  > }

A single address, as nginx or Revproxy send, is the client:

  $ get -H 'X-Forwarded-For: 192.0.2.1' http://127.0.0.1:$PORT1/index.html
  200
  IP: forwarded:192.0.2.1

The <ip> conditions test this address:

  $ get -H 'X-Forwarded-For: 10.1.2.3' http://127.0.0.1:$PORT1/index.html
  403
  IP: forwarded:10.1.2.3

With several addresses, the client is the last one, which the proxy added. The
others come from the client, which can choose them, as nginx keeps them with
$proxy_add_x_forwarded_for:

  $ get -H 'X-Forwarded-For: 10.1.2.3, 192.0.2.2' \
  >   http://127.0.0.1:$PORT1/index.html
  200
  IP: forwarded:192.0.2.2

A value that is not an address matches no <ip>:

  $ get -H 'X-Forwarded-For: unknown' http://127.0.0.1:$PORT1/index.html
  200
  IP: forwarded:unknown

The port that some proxies add is removed:

  $ get -H 'X-Forwarded-For: 10.1.2.3:4711' http://127.0.0.1:$PORT1/index.html
  403
  IP: forwarded:10.1.2.3
  $ get -H 'X-Forwarded-For: [2001:db8::2]:4711' \
  >   http://127.0.0.1:$PORT1/index.html
  200
  IP: forwarded:2001:db8::2

Behind a chain of proxies, the addresses of the trusted ones, given in the
proxies attribute, are skipped from the right:

  $ get -H 'X-Forwarded-For: 10.1.2.3, 198.51.100.1' \
  >   http://127.0.0.1:$PORT2/index.html
  403
  IP: forwarded:10.1.2.3
  $ get -H 'X-Forwarded-For: 10.1.2.3, 192.0.2.3, [2001:db8::1]:443, 198.51.100.1' \
  >   http://127.0.0.1:$PORT2/index.html
  200
  IP: forwarded:192.0.2.3

A value that is not an address is not a trusted proxy:

  $ get -H 'X-Forwarded-For: 10.1.2.3, unknown, 198.51.100.1' \
  >   http://127.0.0.1:$PORT2/index.html
  200
  IP: forwarded:unknown

When all of them are trusted, the client is the first one:

  $ get -H 'X-Forwarded-For: 198.51.100.2, 198.51.100.1' \
  >   http://127.0.0.1:$PORT2/index.html
  200
  IP: forwarded:198.51.100.2

Empty entries are ignored:

  $ get -H 'X-Forwarded-For: 10.1.2.3, , 198.51.100.1' \
  >   http://127.0.0.1:$PORT2/index.html
  403
  IP: forwarded:10.1.2.3

Several X-Forwarded-For lines make one list, in their order:

  $ get -H "X-Forwarded-For: 10.1.2.3" -H "X-Forwarded-For: 192.0.2.7" \
  >   http://127.0.0.1:$PORT1/index.html
  200
  IP: forwarded:192.0.2.7

Tabs around the commas are white space too (RFC 9110, section 5.6.3):

  $ get -H "$(printf 'X-Forwarded-For: 10.1.2.3\t,\t10.1.2.3')" \
  >   http://127.0.0.1:$PORT1/index.html
  403
  IP: forwarded:10.1.2.3

The check-equal-ip attribute, which required the last address to be the one of
the proxy, is refused, even with other attributes:

  $ refused () {
  >   cat > old.conf <<EOF
  > <ocsigen><server>
  > <port>127.0.0.1:$PORT3</port>
  > <extension findlib-package="ocsigenserver.ext.accesscontrol"/>
  > <host hostfilter="*"><allow-forward-for $1/></host>
  > </server></ocsigen>
  > EOF
  >   ocsigenserver -c old.conf >old.log 2>&1; echo "exit $?"
  >   grep -o 'check-equal-ip is [^:]*' old.log
  > }
  $ refused 'check-equal-ip="true"'
  exit 50
  check-equal-ip is no longer supported in <allow-forward-for/>
  $ refused 'proxies="10.0.0.0/8" check-equal-ip="false"'
  exit 50
  check-equal-ip is no longer supported in <allow-forward-for/>
