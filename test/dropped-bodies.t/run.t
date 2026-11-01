A response that the server drops before sending it still frees what its body
holds: the body of a file response is what closes the file.
../dropped-bodies/server.ml answers file.txt to the paths that end with
"file", and drops this response as the first segment of the path says.

  $ . ../server-test-helpers.sh
  $ echo content > file.txt
  $ run_server ../dropped-bodies/server.exe
  ocsigen:main: [WARNING] Command pipe created
  ocsigen:cohttp: [ERROR] Error while handling request.
                          Failure("drop")
  ocsigen:cohttp: [ERROR] Error while handling request.
                          Failure("drop")
  ocsigen:cohttp: [ERROR] Error while handling request.
                          Failure("drop")
  ocsigen:cohttp: [ERROR] Error while handling request.
                          Failure("drop")
  ocsigen:cohttp: [ERROR] Error while handling request.
                          Failure("drop")
  ocsigen:cohttp: [ERROR] Error while handling request.
                          Failure("drop")
  application: [WARNING] Command received: shutdown
  $ open_files () {
  >   curl --unix-socket ./local.sock -s http://local-test/open-files; }
  $ get () {
  >   curl --unix-socket ./local.sock -s -o /dev/null -w '%{http_code}\n' \
  >     "http://local-test/$1"; }
  $ before=$(open_files)
  $ echo "$before" | grep -qE '^[0-9]+$' && echo counted
  counted

A response that an extension drops, by answering without a page or by
failing:

  $ for path in keep next stop fail fail-later fail-stop; do
  >   get $path/file; get $path/file
  > done
  200
  200
  404
  404
  403
  403
  500
  500
  500
  500
  500
  500
  $ test "$(open_files)" = "$before" && echo released
  released

A response that cohttp drops: it reads the rest of the body of the request
before it writes the response, and this body never comes: one byte is sent
out of the 100 announced. The server closes the connection after its
timeout, without answering:

  $ curl --unix-socket ./local.sock -s -H 'Content-Length: 100' -H 'Expect:' \
  >   --data-binary x http://local-test/keep/file || echo "no answer"
  no answer
  $ for _ in $(seq 1 50); do
  >   test "$(open_files)" = "$before" && break; sleep 0.1
  > done
  $ test "$(open_files)" = "$before" && echo released
  released
