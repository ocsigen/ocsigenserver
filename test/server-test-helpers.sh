# Bash functions that help test ocsigenserver

# Run the server using 'dune exec -- "$@"' and doing the necessary setup.
# The server must listen on the unix-domain socket named "local.sock" and on
# the command-pipe named "local.cmd".
#
# Usage:
#   $ run_server ./test.exe
#   $ curl_ "index.html"
#
run_server ()
{
  mkdir -p log data # Directories that might be required by the server
  dune build "$1"
  # Run the server in the background, cut the datetime out of the log output.
  dune exec -- "$@" 2>&1 | cut -d ' ' -f 4- &
  # Wait for the unix-domain socket and the command-pipe to be created
  local timeout=50 # Don't wait more than 0.5s
  while ! ( [[ -e ./local.sock ]] && [[ -e ./local.cmd ]] ) && (( timeout-- > 0 )); do
    sleep 0.01
  done
  # Print an error if a file is missing
  ls ./local.sock ./local.cmd >/dev/null || return 1
  # Shutdown the server at the end of the test
  trap 'echo shutdown > local.cmd && wait' EXIT
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
  local path=$1; shift
  # Remove the 'date' header, which is unreproducible
  curl --unix-socket ./local.sock -s -i "$@" "http://local-test/$path" | \
    grep -v "^date: "
}
