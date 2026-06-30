# POSIX sh functions that help test ocsigenserver.
#
# These are written for plain /bin/sh (dash) so they run under dune's cram
# runner on every platform, not just where /bin/sh is bash.

# Start a serve-mode server in the background. Usage: start_server COMMAND...;
# sets SERVER_PID. All standard fds are redirected so the server never holds the
# cram output pipe; it is stopped gracefully with shutdown_server, so it exits
# on its own (no process is left behind for dune's cram cleanup).
start_server ()
{
  "$@" >server.log 2>&1 </dev/null &
  SERVER_PID=$!
}

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
  # Build first, redirecting the output: a successful build is silent, but
  # warnings (e.g. the macOS linker's "ignoring duplicate libraries" for zlib)
  # would otherwise pollute the captured test output. Show the log on failure.
  if ! dune build "$1" >build.log 2>&1; then
    cat build.log
    return 1
  fi
  # Run the server in the background. Its logs (which are asynchronous and would
  # otherwise interleave non-deterministically with the curl output) go to a
  # file; use 'server_log' to inspect them.
  dune exec -- "$@" >server.log 2>&1 &
  # Wait for the unix-domain socket and the command-pipe to be created. Allow a
  # generous timeout: startup can take a few seconds on a loaded CI runner.
  timeout=1000 # Don't wait more than ~10s
  while ! { [ -e ./local.sock ] && [ -e ./local.cmd ]; } && [ "$timeout" -gt 0 ]; do
    timeout=$((timeout - 1))
    sleep 0.01
  done
  # If a file is still missing the server failed to start; show its log to make
  # the failure diagnosable, then abort.
  if ! { [ -e ./local.sock ] && [ -e ./local.cmd ]; }; then
    echo "server did not start; server.log:"
    server_log
    return 1
  fi
  # Shutdown the server at the end of the test
  trap 'echo shutdown > local.cmd && wait' EXIT
}

# Print the server log with the unreproducible datetime prefix removed.
server_log ()
{
  cut -b 18- server.log
}

# Gracefully stop a serve-mode server: send "shutdown" to its command pipe and
# wait for it to exit on its own. Because the server exits itself, no process is
# left behind for dune's (Windows-unreliable) cram cleanup to hang on.
# Usage: shutdown_server COMMAND-PIPE-NAME PID
shutdown_server ()
{
  name=$1
  pid=$2
  # Only send if the server is still running: opening the pipe when the server
  # (its only reader) is already gone would block forever. (This guards against
  # the EXIT trap firing after the server has exited.) The ocsigenserver binary
  # itself is the client: "--command shutdown" opens the command pipe (a Unix
  # FIFO or a Windows named pipe) and writes the command. Using the binary avoids
  # the Cygwin shell's inability to open a Windows named pipe.
  if kill -0 "$pid" 2>/dev/null; then
    ocsigenserver --command shutdown --command-pipe "$name" >/dev/null 2>&1
  fi
  wait "$pid" 2>/dev/null || true
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
  # Remove the 'date' header, which is unreproducible
  curl --unix-socket ./local.sock --user-agent "" -s -i \
    "$@" "http://local-test/$path" | \
    grep -v "^date: "
}
