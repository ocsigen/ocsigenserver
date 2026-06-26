Serve a directory with a single command, with no configuration file and no
pre-created log directory.

  $ mkdir www
  $ printf '<html>hello</html>' > www/index.html

Start the server in serve mode on a TCP port. Logs go to stderr (redirected to
a file here), so no log directory is created.

  $ ocsigenserver --serve www --port 8061 >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT

An existing file is served with the correct content type and body. The headers
that are not reproducible (the date, and the validators derived from the file's
modification time) are filtered out.

  $ curl -s -i --max-time 10 --retry 20 --retry-delay 1 --retry-connrefused \
  >   --user-agent "" http://127.0.0.1:8061/index.html \
  >   | grep -viE "^(date|etag|last-modified): "
  HTTP/1.1 200 OK
  content-type: text/html
  server: Ocsigen
  content-length: 18
  
  <html>hello</html>


A missing file gives a 404.

  $ curl -s -o /dev/null -w "%{http_code}\n" --max-time 10 \
  >   http://127.0.0.1:8061/nope.html
  404

The server logged a startup banner with the URL:

  $ grep -o 'on http://localhost:8061' server.log
  on http://localhost:8061

No log directory is created in the current directory.

  $ ls
  server.log
  www
