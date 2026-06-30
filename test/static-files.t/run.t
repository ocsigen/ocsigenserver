Static file resolution: index files, nested paths, directory listing and
protection against escaping the served directory.

  $ . ../server-test-helpers.sh
  $ mkdir -p www/sub
  $ printf '<html>root index</html>' > www/index.html
  $ printf '<html>nested</html>' > www/sub/page.html
  $ printf 'secret' > secret.txt
  $ run_server ./test.exe

The directory root resolves to its index file.

  $ curl_ ""
  HTTP/1.1 200 OK
  content-type: text/html
  server: Ocsigen
  content-length: 23
  
  <html>root index</html>

A nested file is served.

  $ curl_ sub/page.html
  HTTP/1.1 200 OK
  content-type: text/html
  server: Ocsigen
  content-length: 19
  
  <html>nested</html>

A directory without an index is listed (listing is enabled). The markup
contains unreproducible sizes and dates, so only the status line and the
presence of the entry are checked.

  $ curl_ sub/ | head -1
  HTTP/1.1 200 OK
  $ curl_ sub/ | grep -c '>page.html<'
  1

A missing file gives a 404.

  $ curl_ sub/missing.html | head -1
  HTTP/1.1 404 Not Found

A request that tries to escape the served directory with ".." does not reach
files outside it (curl is told to send the path as-is).

  $ curl_ ../secret.txt --path-as-is | head -1
  HTTP/1.1 404 Not Found
  $ curl_ %2e%2e/secret.txt --path-as-is | head -1
  HTTP/1.1 404 Not Found
