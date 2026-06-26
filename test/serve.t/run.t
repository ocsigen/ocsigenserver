Serve a directory with a single command, with no configuration file and no
pre-created log directory.

  $ mkdir www
  $ printf '<html>hello</html>' > www/index.html

Start the server in serve mode on a TCP port. Logs go to stderr (redirected to
a file here), so no log directory is created.

  $ ocsigenserver --serve www --port 8061 >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT

An existing file is served with the correct status, content type and body:

  $ curl -s -D headers -w '%{http_code}\n' --retry 20 --retry-delay 1 \
  >   --retry-connrefused http://127.0.0.1:8061/index.html
  <html>hello</html> (no-eol)
  200
  $ grep -i '^content-type:' headers | tr -d '\r'
  content-type: text/html

The safe-by-default security headers are present:

  $ grep -i '^x-content-type-options:' headers | tr -d '\r'
  x-content-type-options: nosniff
  $ grep -i '^x-frame-options:' headers | tr -d '\r'
  x-frame-options: SAMEORIGIN
  $ grep -i '^referrer-policy:' headers | tr -d '\r'
  referrer-policy: strict-origin-when-cross-origin

A missing file gives a 404.

  $ curl -s -o /dev/null -w "%{http_code}\n" http://127.0.0.1:8061/nope.html
  404

The server logged a startup banner with the URL:

  $ grep -o 'on http://localhost:8061' server.log
  on http://localhost:8061

No log directory is created in the current directory.

  $ ls
  headers
  server.log
  www
