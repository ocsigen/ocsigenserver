<allow-forward-for/> takes the client address from X-Forwarded-For. An <ip>
condition evaluated afterwards tests and logs the address the server retained.

  $ mkdir -p www log data
  $ printf 'hello\n' > www/index.html
  $ cat > site.conf <<'EOF'
  > <ocsigen><server>
  > <port>8082</port>
  > <port>8083</port>
  > <logdir>log</logdir><datadir>data</datadir>
  > <loglevel source="ocsigen:ext:access-control" level="info"/>
  > <extension findlib-package="ocsigenserver.ext.staticmod"/>
  > <extension findlib-package="ocsigenserver.ext.accesscontrol"/>
  > <host hostfilter="*">
  >   <if><port value="8082"/>
  >     <then><allow-forward-for/></then>
  >     <else><allow-forward-for check-equal-ip="true"/></else>
  >   </if>
  >   <if><ip value="10.0.0.0/8"/><then><forbidden/></then></if>
  >   <static dir="www" />
  > </host>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c site.conf >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT
  $ get () {
  >   seen=$(grep -c 'IP: ' server.log)
  >   curl -s -o /dev/null -w '%{http_code}\n' --retry 20 --retry-delay 1 \
  >     --retry-connrefused "$@"
  >   grep -o 'IP: [^ ]*' server.log | tail -n +$((seen + 1))
  > }

A single address, as nginx or Revproxy send, is the client:

  $ get -H 'X-Forwarded-For: 192.0.2.1' http://127.0.0.1:8082/index.html
  200
  IP: forwarded:192.0.2.1

With several addresses, the first one is the client:

  $ get -H 'X-Forwarded-For: 192.0.2.2, 198.51.100.1' \
  >   http://127.0.0.1:8082/index.html
  200
  IP: forwarded:192.0.2.2

The <ip> conditions test this address:

  $ get -H 'X-Forwarded-For: 10.1.2.3' http://127.0.0.1:8082/index.html
  403
  IP: forwarded:10.1.2.3

A value that is not an address matches no <ip>:

  $ get -H 'X-Forwarded-For: unknown' http://127.0.0.1:8082/index.html
  200
  IP: forwarded:unknown

The port that some proxies add is removed:

  $ get -H 'X-Forwarded-For: 10.1.2.3:4711' http://127.0.0.1:8082/index.html
  403
  IP: forwarded:10.1.2.3
  $ get -H 'X-Forwarded-For: [2001:db8::1]:4711' \
  >   http://127.0.0.1:8082/index.html
  200
  IP: forwarded:2001:db8::1

With check-equal-ip, the last address must be the one of the proxy that sent
the request:

  $ get -H 'X-Forwarded-For: 192.0.2.3, 127.0.0.1' \
  >   http://127.0.0.1:8083/index.html
  200
  IP: forwarded:192.0.2.3

Otherwise, the header is ignored, and the client is the peer:

  $ get -H 'X-Forwarded-For: 192.0.2.4' http://127.0.0.1:8083/index.html
  200
  IP: 127.0.0.1
  $ get -H 'X-Forwarded-For: 192.0.2.5, 198.51.100.1' \
  >   http://127.0.0.1:8083/index.html
  200
  IP: 127.0.0.1
  $ get -H 'X-Forwarded-For: 192.0.2.6, not-an-ip' \
  >   http://127.0.0.1:8083/index.html
  200
  IP: 127.0.0.1
