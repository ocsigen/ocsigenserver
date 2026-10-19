The address of <port> chooses the families the server listens on. Needs IPv6
on the loopback interface. Self-contained POSIX sh.

  $ mkdir -p log data
  $ cat > conf.xml <<EOF
  > <ocsigen><server>
  >   <port>8110</port>
  >   <port>[::]:8111</port>
  >   <port>*:8112</port>
  >   <logdir>log</logdir><datadir>data</datadir>
  >   <host hostfilter="*"></host>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c conf.xml >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT
  $ curl -s -o /dev/null --max-time 10 --retry 20 --retry-delay 1 \
  >   --retry-connrefused http://127.0.0.1:8110/
  $ try () {
  >   if curl -s -o /dev/null --max-time 5 "$1"; then echo yes; else echo no; fi
  > }

A port alone listens on IPv4 and IPv6:

  $ try http://127.0.0.1:8110/; try 'http://[::1]:8110/'
  yes
  yes

[::] listens on IPv6 only, whatever the default of the system:

  $ try http://127.0.0.1:8111/; try 'http://[::1]:8111/'
  no
  yes

* listens on IPv4 only:

  $ try http://127.0.0.1:8112/; try 'http://[::1]:8112/'
  yes
  no
