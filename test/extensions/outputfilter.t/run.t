Outputfilter adds a header to responses.

  $ . ../../server-test-helpers.sh
  $ printf 'data' > index.html
  $ run_server ./test.exe

The configured header is present on the response.

  $ curl_ index.html | grep -i '^x-test:'
  x-test: hello
