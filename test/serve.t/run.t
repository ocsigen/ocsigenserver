Serve a directory with a single command, with no configuration file and no
pre-created log directory.

  $ mkdir www
  $ printf '<html>hello</html>' > www/index.html

Start the server in serve mode on a TCP port. Logs go to stderr (redirected to
a file here), so no log directory is created.

  $ mkdir tmp
  $ TMPDIR=$PWD/tmp ocsigenserver --serve www --port 8061 >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT

An existing file is served with the correct status, content type and body:

  $ curl -s -D headers -o body -w '%{http_code}\n' --max-time 10 \
  >   --retry 20 --retry-delay 1 --retry-connrefused \
  >   http://127.0.0.1:8061/index.html
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
  >   http://127.0.0.1:8061/index.html | grep -iE "^(HTTP|content-encoding)"
  HTTP/1.1 200 OK
  content-encoding: gzip

A missing file gives a 404.

  $ curl -s -o /dev/null -w "%{http_code}\n" --max-time 10 \
  >   http://127.0.0.1:8061/nope.html
  404

The server logged a startup banner with the URL:

  $ grep -o 'on http://127.0.0.1:8061' server.log
  on http://127.0.0.1:8061

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

  $ ocsigenserver --serve www --port 8084 --address 0.0.0.0 \
  >   >server-any.log 2>&1 &
  $ ANY_PID=$!
  $ curl -s -o /dev/null -w '%{http_code}\n' --max-time 10 --retry 20 \
  >   --retry-delay 1 --retry-connrefused http://127.0.0.1:8084/index.html
  200
  $ grep -o 'on http://0.0.0.0:8084 (all interfaces)' server-any.log
  on http://0.0.0.0:8084 (all interfaces)
  $ kill $ANY_PID

An address that is not an IP address is refused:

  $ ocsigenserver --serve www --address localhost >error.log 2>&1; echo "exit $?"
  exit 2
  $ grep -o -- '--address: localhost is not an IP address' error.log
  --address: localhost is not an IP address
