Outputfilter adds a header to responses.

  $ . ../../server-test-helpers.sh
  $ printf 'data' > index.html
  $ run_server ../outputfilter_server.exe

The configured header is present on the response.

  $ curl_ index.html | grep -i '^x-test:'
  x-test: hello
