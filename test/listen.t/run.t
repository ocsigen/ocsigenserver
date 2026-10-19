The address of <port> chooses the families the server listens on. Needs IPv6
on the loopback interface. Self-contained POSIX sh.

  $ . ../server-test-helpers.sh
  $ free_ports 3
  $ mkdir -p log
  $ cat > conf.xml <<EOF
  > <ocsigen><server>
  >   <port>$PORT1</port>
  >   <port>[::]:$PORT2</port>
  >   <port>*:$PORT3</port>
  >   <logdir>log</logdir>
  >   <host hostfilter="*"></host>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c conf.xml >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT
  $ wait_server $SERVER_PID server.log http://127.0.0.1:$PORT1/ \
  >   "http://[::1]:$PORT2/" http://127.0.0.1:$PORT3/
  $ try () {
  >   if curl -s -o /dev/null --max-time 5 "$1"; then echo yes; else echo no; fi
  > }

A port alone listens on IPv4 and IPv6:

  $ try http://127.0.0.1:$PORT1/; try "http://[::1]:$PORT1/"
  yes
  yes

[::] listens on IPv6 only, whatever the default of the system:

  $ try http://127.0.0.1:$PORT2/; try "http://[::1]:$PORT2/"
  no
  yes

* listens on IPv4 only:

  $ try http://127.0.0.1:$PORT3/; try "http://[::1]:$PORT3/"
  yes
  no
