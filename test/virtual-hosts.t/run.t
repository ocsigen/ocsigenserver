Virtual hosts match host names case-insensitively, and ignore a final dot
(fully qualified form), in the Host header as in the hostfilter attribute.

  $ . ../server-test-helpers.sh
  $ free_ports 1
  $ trap 'kill $CONF_PID 2>/dev/null; stop_server $LIB_PID' EXIT
  $ mkdir -p log admin www
  $ echo admin > admin/index.html && echo public > www/index.html
  $ hosts () {
  >   hosts_opts=$1 hosts_url=$2; shift 2
  >   for h in "$@"; do
  >     printf '%s: ' "$h"
  >     curl -s $hosts_opts --max-time 10 -H "Host: $h" "$hosts_url"
  >   done
  > }

With a configuration file, whose hostfilter ends with a dot:

  $ cat > conf.xml <<EOF
  > <ocsigen><server>
  >   <port>127.0.0.1:$PORT1</port>
  >   <logdir>log</logdir>
  >   <extension findlib-package="ocsigenserver.ext.staticmod"/>
  >   <host hostfilter="admin.test."><static dir="admin"/></host>
  >   <host hostfilter="*"><static dir="www"/></host>
  > </server></ocsigen>
  > EOF
  $ ocsigenserver -c conf.xml >conf.log 2>&1 &
  $ CONF_PID=$!
  $ wait_server $CONF_PID conf.log http://127.0.0.1:$PORT1/
  $ hosts "" http://127.0.0.1:$PORT1/ \
  >   admin.test admin.test. ADMIN.Test. admin.test.. other.test
  admin.test: admin
  admin.test.: admin
  ADMIN.Test.: admin
  admin.test..: public
  other.test: public
  $ kill $CONF_PID

With hosts given to Ocsigen.Server.host ~regexp:"admin\\.test$":

  $ ../virtual-hosts/test.exe >lib.log 2>&1 &
  $ LIB_PID=$!
  $ wait_server $LIB_PID lib.log local.sock local.cmd
  $ hosts "--unix-socket local.sock" http://x/ \
  >   admin.test ADMIN.Test admin.test. other.test
  admin.test: admin
  ADMIN.Test: admin
  admin.test.: admin
  other.test: public
