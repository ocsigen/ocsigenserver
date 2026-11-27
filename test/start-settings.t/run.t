The default charset, ports, upload directory and maximum upload size given to
Ocsigen.Server.start apply to the hosts given to it, which OCaml evaluates
before it. A host that gives its own keeps it.

  $ . ../server-test-helpers.sh
  $ free_ports 1
  $ mkdir -p log www

The instructions of a host are applied by Ocsigen.Server.start too, which
stops the server with their error, as for an error in a configuration file:

  $ ../start-settings/test.exe $PORT1 --failing-instruction >failing.log 2>&1
  [100]
  $ grep -o 'Fatal - .*' failing.log
  Fatal - Uncaught exception: Failure("this instruction fails")

The server with the settings:

  $ trap 'stop_server $SERVER_PID' EXIT
  $ echo x > www/a.txt
  $ ../start-settings/test.exe $PORT1 >server.log 2>&1 &
  $ SERVER_PID=$!
  $ wait_server $SERVER_PID server.log local.sock local.cmd \
  >   http://127.0.0.1:$PORT1/
  $ get () {
  >   get_host=$1 get_path=$2; shift 2
  >   curl -s --unix-socket local.sock -H "Host: $get_host" --max-time 10 \
  >     "$@" "http://x/$get_path" | sed "s/: $PORT1\$/: PORT1/"
  > }

The settings of Ocsigen.Server.start, with as default port the one of the TCP
socket, not of the Unix-domain one, given first with port 0:

  $ get default settings
  default port: PORT1
  charset: iso-8859-15
  uploaddir: upload
  maxuploadfilesize: 1234
  $ get default a.txt -D - -o /dev/null | grep -i '^content-type' | tr -d '\r'
  content-type: text/plain; charset=iso-8859-15

Those of the host:

  $ get own settings
  default port: 1234
  charset: utf-8
  uploaddir: none
  maxuploadfilesize: 1234
  $ get own a.txt -D - -o /dev/null | grep -i '^content-type' | tr -d '\r'
  content-type: text/plain; charset=utf-8
