Serve a directory with a single command, with no configuration file and no
pre-created log directory.

  $ . ../server-test-helpers.sh
  $ mkdir www
  $ printf '<html>hello</html>' > www/index.html

Start the server in serve mode on a TCP port. Logs go to stderr (redirected to
a file here), so no log directory is created.

  $ start_server ocsigenserver --serve www --port 8061 --command-pipe .serve-cmd
  $ trap 'shutdown_server .serve-cmd $SERVER_PID' EXIT

An existing file is served with the correct content type and body. The
unreproducible "date" header is filtered out.

  $ curl -s -i --retry 20 --retry-delay 1 --retry-connrefused --user-agent "" \
  >   http://127.0.0.1:8061/index.html | grep -v "^date: "
  HTTP/1.1 200 OK
  content-type: text/html
  server: Ocsigen
  content-length: 18
  
  <html>hello</html>


A missing file gives a 404. (The body is written to a file rather than
/dev/null, which the native Windows curl cannot open; it is removed before the
directory listing below.)

  $ curl -s -o body -w "%{http_code}\n" http://127.0.0.1:8061/nope.html
  404

A directory with no index file gives a 404 too, since directory listing is off
by default.

  $ mkdir www/empty
  $ curl -s -o body -w "%{http_code}\n" http://127.0.0.1:8061/empty/
  404

No log directory is created in the current directory.

  $ rm -f body
  $ ls
  server.log
  www

The server is stopped gracefully on exit by the EXIT trap above (it sends
"shutdown" to the command pipe and waits for the server to exit on its own).
