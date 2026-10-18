Staticmod with a configuration file.

  $ mkdir www
  $ echo index > www/index.html
  $ cat > site.conf <<'CONF'
  > <ocsigen><server>
  > <port>8085</port>
  > <extension findlib-package="ocsigenserver.ext.staticmod"/>
  > <host hostfilter="*">
  >   <static regexp="~([^/]*)(.*)" dest="/nonexistent/\1\2" />
  >   <static dir="www" />
  > </host>
  > </server></ocsigen>
  > CONF
  $ ocsigenserver -c site.conf >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT

A <static regexp> that does not match the path lets the next one serve it:

  $ curl -s -w ' %{http_code}\n' --max-time 10 --retry 20 --retry-delay 1 \
  >   --retry-connrefused http://127.0.0.1:8085/index.html
  index
   200
