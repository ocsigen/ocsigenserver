Rewritemod rewrites the request path internally (without a redirect).

  $ . ../../server-test-helpers.sh
  $ printf 'real content' > page.html
  $ run_server ../rewritemod_server.exe

A request under the "alias/" prefix is rewritten and served from the real
path, with a 200 (no redirect).

  $ curl_ alias/page.html
  HTTP/1.1 200 OK
  content-type: text/html
  server: Ocsigen
  content-length: 12
  
  real content
