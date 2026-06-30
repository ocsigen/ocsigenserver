Redirectmod redirects requests matching a regexp.

  $ . ../../server-test-helpers.sh
  $ printf 'served by staticmod' > target.html
  $ run_server ./test.exe

A request matching the rule gets a permanent redirect to the rewritten
location.

  $ curl_ old/target.html
  HTTP/1.1 301 Moved Permanently
  Location: new/target.html
  server: Ocsigen
  content-length: 0
  

A request that does not match is served normally.

  $ curl_ target.html
  HTTP/1.1 200 OK
  content-type: text/html
  server: Ocsigen
  content-length: 19
  
  served by staticmod
