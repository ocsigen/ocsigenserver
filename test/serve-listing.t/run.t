Serve mode with directory listing enabled: a directory without an index file is
listed, and the files in it are served.

  $ . ../server-test-helpers.sh
  $ mkdir www
  $ printf 'hello' > www/a.txt
  $ start_server ocsigenserver --serve www --port 8070 --directory-listing --command-pipe .listing-cmd
  $ trap 'shutdown_server .listing-cmd $SERVER_PID' EXIT

The directory without an index file returns a 200 listing that mentions the
file. (The body is written to a file rather than /dev/null, which the native
Windows curl cannot open.)

  $ curl -s -o body -w "%{http_code}\n" --retry 20 --retry-delay 1 \
  >   --retry-connrefused --user-agent "" http://127.0.0.1:8070/
  200
  $ grep -q a.txt body && echo "listing mentions a.txt"
  listing mentions a.txt

The file itself is served.

  $ curl -s -o body -w "%{http_code}\n" http://127.0.0.1:8070/a.txt
  200

The server is stopped gracefully on exit by the EXIT trap above (it sends
"shutdown" to the command pipe and waits for the server to exit on its own).
