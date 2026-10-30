Serve a directory with a single command, with no configuration file and no
pre-created log directory.

  $ . ../server-test-helpers.sh
  $ free_ports 2
  $ mkdir www
  $ printf '<html>hello</html>' > www/index.html
  $ mkdir -p www/.git www/.well-known www/sub
  $ echo secret > www/.env; echo secret > www/.git/config
  $ echo secret > www/sub/.htpasswd; echo contact > www/.well-known/security.txt

Start the server in serve mode on a TCP port. Logs go to stderr (redirected to
a file here), so no log directory is created.

  $ mkdir tmp
  $ TMPDIR=$PWD/tmp ocsigenserver --serve www --port $PORT1 >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT
  $ wait_server $SERVER_PID server.log http://127.0.0.1:$PORT1/index.html

An existing file is served with the correct status, content type and body:

  $ curl -s -D headers -o body -w '%{http_code}\n' --max-time 10 \
  >   http://127.0.0.1:$PORT1/index.html
  200
  $ cat body; echo
  <html>hello</html>
  $ grep -i '^content-type:' headers | tr -d '\r'
  content-type: text/html

The safe-by-default security headers are present:

  $ grep -i '^x-content-type-options:' headers | tr -d '\r'
  x-content-type-options: nosniff
  $ grep -i '^x-frame-options:' headers | tr -d '\r'
  x-frame-options: SAMEORIGIN
  $ grep -i '^referrer-policy:' headers | tr -d '\r'
  referrer-policy: strict-origin-when-cross-origin

Compression is on by default in serve mode: a client that accepts an encoding
gets a compressed response.

  $ curl -s -i --user-agent "" -H "Accept-Encoding: gzip" --compressed \
  >   http://127.0.0.1:$PORT1/index.html | grep -iE "^(HTTP|content-encoding)"
  HTTP/1.1 200 OK
  content-encoding: gzip

A missing file gives a 404.

  $ curl -s -o /dev/null -w "%{http_code}\n" --max-time 10 \
  >   http://127.0.0.1:$PORT1/nope.html
  404

Files and directories whose name starts with a dot are not served, wherever
they are, except /.well-known/ (RFC 8615):

  $ for p in .env .git/config sub/.htpasswd .well-known/security.txt; do
  >   curl -s -o /dev/null -w "$p %{http_code}\n" --max-time 10 \
  >     http://127.0.0.1:$PORT1/$p
  > done
  .env 404
  .git/config 404
  sub/.htpasswd 404
  .well-known/security.txt 200

The server logged a startup banner with the URL:

  $ grep -o "on http://127.0.0.1:$PORT1" server.log | sed "s/:$PORT1/:PORT/"
  on http://127.0.0.1:PORT

No log directory is created in the current directory.

  $ ls
  body
  headers
  server.log
  tmp
  www

No command pipe is created in the temporary directory, where this mode used to
put it (nor in the current directory, listed above).

  $ ls tmp

The server listens on the loopback interface by default (see the banner
above). --address chooses another address:

  $ ocsigenserver --serve www --port $PORT2 --address 0.0.0.0 \
  >   >server-any.log 2>&1 &
  $ ANY_PID=$!
  $ wait_server $ANY_PID server-any.log http://127.0.0.1:$PORT2/index.html
  $ curl -s -o /dev/null -w '%{http_code}\n' --max-time 10 \
  >   http://127.0.0.1:$PORT2/index.html
  200
  $ grep -o "on http://0.0.0.0:$PORT2 (all interfaces)" server-any.log \
  >   | sed "s/:$PORT2/:PORT/"
  on http://0.0.0.0:PORT (all interfaces)
  $ kill $ANY_PID

An address that is not an IP address is refused:

  $ ocsigenserver --serve www --address localhost >error.log 2>&1; echo "exit $?"
  exit 2
  $ grep -o -- '--address: localhost is not an IP address' error.log
  --address: localhost is not an IP address
