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
  >   curl --unix-socket ./local.sock -s --max-time 10 \
  >     http://local-test/open-files; }
  $ get () {
  >   curl --unix-socket ./local.sock -s --max-time 10 -o /dev/null \
  >     -w '%{http_code}' "http://local-test/$1"; }
  $ before=$(open_files)
  $ echo "$before" | grep -qE '^[0-9]+$' && echo counted
  counted

Each path is requested twice, then the open files are counted again, for up
to 2 seconds, as a file may be closed just after its response is sent. They
are counted afresh after a leak, so that each case is checked on its own:

  $ released () {
  >   for _ in $(seq 1 20); do
  >     test "$(open_files)" = "$before" && return 0; sleep 0.1
  >   done; return 1; }
  $ drop () {
  >   echo "$1: $(get $1) $(get $1)"
  >   released || { echo "  a file is left open"; before=$(open_files); }; }

A response that an extension drops, by answering without a page or by
failing:

  $ for path in keep next continue retry stop stop-site stop-host \
  >   fail fail-later fail-stop fail-continue; do
  >   drop $path/file
  > done
  keep/file: 200 200
  next/file: 404 404
  continue/file: 404 404
  retry/file: 200 200
  stop/file: 403 403
  stop-site/file: 403 403
  stop-host/file: 403 403
  fail/file: 500 500
  fail-later/file: 500 500
  fail-stop/file: 500 500
  fail-continue/file: 500 500

The same, when the extension that drops the response is composed in
another one:

  $ for path in keep next stop fail; do drop $path/nested/file; done
  keep/nested/file: 200 200
  next/nested/file: 404 404
  stop/nested/file: 403 403
  fail/nested/file: 500 500
