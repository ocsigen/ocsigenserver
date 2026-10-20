# POSIX shell functions that help test ocsigenserver.
# Kept free of bashisms: dune runs cram tests with /bin/sh, which is dash on
# Debian and Ubuntu.

# Run the server "$@" in the background and do the necessary setup. The
# server must listen on the unix-domain socket named "local.sock" and on the
# command-pipe named "local.cmd".
#
# Usage:
#   $ run_server ../deflatemod/test.exe
#   $ curl_ "index.html"
#
run_server ()
{
  mkdir -p log # Directory that might be required by the server
  # Run the server in the background, cut the datetime out of the log output.
  # Access lines (Combined Log Format) carry their own timestamp, which is not
  # reproducible, so drop them from the captured log.
  "$@" 2>&1 | cut -b 18- | grep -v "^ocsigen:access:" &
  # Wait for the unix-domain socket and the command-pipe to be created
  timeout=50 # Don't wait more than 0.5s
  while ! { [ -e ./local.sock ] && [ -e ./local.cmd ]; } && [ "$timeout" -gt 0 ]; do
    timeout=$((timeout - 1))
    sleep 0.01
  done
  # Print an error if a file is missing
  ls ./local.sock ./local.cmd >/dev/null || return 1
  # Shutdown the server at the end of the test
  trap 'echo shutdown > local.cmd && wait' EXIT
}

# Wait until the server answers HTTP requests at URL, whatever the status
# code. Polls every 20 ms, for about 10 s at most. curl's own --retry waits a
# whole second before its first retry, hence this loop. The arguments are
# passed to curl, so options may precede the URL (-k for a self-signed
# certificate).
#
# Usage:
#   $ ocsigenserver --serve www --port 8061 >server.log 2>&1 &
#   $ wait_http http://127.0.0.1:8061/
#
wait_http ()
{
  wait_http_deadline=$(($(date +%s) + 10))
  while ! curl -s -o /dev/null --max-time 2 "$@"; do
    if [ "$(date +%s)" -gt "$wait_http_deadline" ]; then
      echo "wait_http: $* does not answer" >&2
      return 1
    fi
    sleep 0.02
  done
}

# Wrapper around 'curl' that connects to the server. First argument is the
# request path, the other arguments are directly passed to 'curl'.
#
# Usage:
#   $ curl_ ""
#   $ curl_ "index.html"
#   $ curl_ "index.html" --no-show-headers   # Supress the headers
#
curl_ ()
{
  path=$1; shift
  # Remove the headers that are not reproducible: 'date', and the validators
  # of a static file, which are derived from its modification time.
  curl --unix-socket ./local.sock --user-agent "" -s -i \
    "$@" "http://local-test/$path" | \
    grep -viE "^(date|etag|last-modified): "
}
