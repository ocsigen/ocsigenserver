Accesses are logged in the Apache/nginx Combined Log Format.

  $ mkdir www
  $ printf '<html>hello</html>' > www/index.html

Start the server in serve mode. Access lines go to stdout (redirected here to a
file), with no date/source prefix so the standard format is preserved.

  $ ocsigenserver --serve www --port 8062 >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT

Request a file with a known User-Agent and Referer.

  $ curl -s -o /dev/null --retry 20 --retry-delay 1 --retry-connrefused \
  >   --user-agent "test-agent" --referer "http://example.test/" \
  >   http://127.0.0.1:8062/index.html

The access line follows the Combined Log Format:
%h %l %u %t "%r" %>s %b "%{Referer}i" "%{User-agent}i". The timestamp is masked
because it is not reproducible. There is no authenticated user (%u is "-"), and
%b is the response size (18 bytes, from Content-Length).

  $ for _ in $(seq 1 20); do
  >   grep -q 'GET /index.html' server.log && break
  >   sleep 0.5
  > done
  $ grep 'GET /index.html' server.log | sed 's/\[[^]]*\]/[TIME]/'
  127.0.0.1 - - [TIME] "GET /index.html HTTP/1.1" 200 18 "http://example.test/" "test-agent"

A missing file is logged too, with the 404 status and "-" for the absent
Referer and User-Agent.

  $ curl -s -o /dev/null --user-agent "" http://127.0.0.1:8062/nope.html
  $ for _ in $(seq 1 20); do
  >   grep -q 'GET /nope.html' server.log && break
  >   sleep 0.5
  > done
  $ grep 'GET /nope.html' server.log | sed 's/\[[^]]*\]/[TIME]/'
  127.0.0.1 - - [TIME] "GET /nope.html HTTP/1.1" 404 16 "-" "-"
