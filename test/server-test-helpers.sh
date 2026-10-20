# POSIX shell functions that help test ocsigenserver.
# Kept free of bashisms: dune runs cram tests with /bin/sh, which is dash on
# Debian and Ubuntu.

# Run the server "$@" in the background and do the necessary setup. The
# server must listen on the unix-domain socket named "local.sock" and on the
# command-pipe named "local.cmd". Its process id is in SERVER_PID, and it is
# shut down at the end of the test.
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
  # reproducible, so drop them from the captured log. The output goes through
  # a named pipe, so that $! is the process id of the server.
  mkfifo server.out
  "$@" >server.out 2>&1 &
  SERVER_PID=$!
  cut -b 18- <server.out | grep -v "^ocsigen:access:" &
  wait_server "$SERVER_PID" - ./local.sock ./local.cmd || return 1
  # Shutdown the server at the end of the test, and wait for its output
  trap 'stop_server $SERVER_PID; wait' EXIT
}

# Wait until the server of process id PID is ready: until each FILE exists
# (its socket, its command pipe) and each URL (starting with http:// or
# https://) answers, whatever the status code. If the server exits first, or
# is not ready after about 10 s, print its log LOG ("-" for none) and fail.
#
# Usage:
#   $ ../security-tls/test.exe >server.log 2>&1 &
#   $ SERVER_PID=$!
#   $ wait_server $SERVER_PID server.log local.cmd https://127.0.0.1:8453/
#
wait_server ()
{
  wait_server_pid=$1 wait_server_log=$2
  shift 2
  wait_server_deadline=$(($(date +%s) + 10))
  while kill -0 "$wait_server_pid" 2>/dev/null; do
    wait_server_ready=true
    for wait_server_c in "$@"; do
      case $wait_server_c in
        http://*|https://*)
          curl -sk -o /dev/null --max-time 2 "$wait_server_c" ;;
        *) [ -e "$wait_server_c" ] ;;
      esac || { wait_server_ready=false; break; }
    done
    # The server may have failed after creating its files, and another
    # server may answer on the same port: check that it still runs.
    if $wait_server_ready && kill -0 "$wait_server_pid" 2>/dev/null; then
      return 0
    fi
    [ "$(date +%s)" -gt "$wait_server_deadline" ] && break
    sleep 0.05
  done
  if kill -0 "$wait_server_pid" 2>/dev/null; then
    echo "wait_server: the server is not ready: $*" >&2
  else
    echo "wait_server: the server is not running" >&2
  fi
  [ "$wait_server_log" = - ] || cat "$wait_server_log" >&2
  return 1
}

# Write COMMAND to the command pipe PIPE of the server of process id PID.
# Opening a named pipe for writing blocks until a reader opens it: the
# command is written by a background process, killed if the server exits
# before reading it, or has not read it after about 10 s.
#
# Usage:
#   $ send_command $SERVER_PID local.cmd reload
#
send_command ()
{
  if [ ! -p "$2" ]; then
    echo "send_command: $2 is not a named pipe" >&2
    return 1
  fi
  echo "$3" 2>/dev/null >"$2" &
  send_command_writer=$!
  send_command_i=0
  while kill -0 "$send_command_writer" 2>/dev/null \
        && kill -0 "$1" 2>/dev/null && [ "$send_command_i" -lt 200 ]; do
    send_command_i=$((send_command_i + 1))
    sleep 0.05
  done
  kill "$send_command_writer" 2>/dev/null
  # The status of the writer tells whether the command was written, even
  # when the server exits right after reading it.
  wait "$send_command_writer" 2>/dev/null && return 0
  if kill -0 "$1" 2>/dev/null; then
    echo "send_command: the server does not read $2" >&2
  else
    echo "send_command: the server is not running, \"$3\" not sent" >&2
  fi
  return 1
}

# Shut the server of process id PID down through its command pipe PIPE
# ("local.cmd" by default), and wait for it to exit. If it is not running,
# print a message instead of blocking on the pipe. If it does not exit
# within about 10 s, kill it. Fails when the server could not be shut down
# cleanly. In an EXIT trap, the messages go to the output of dune, not to
# the diff of the test.
#
# Usage:
#   $ ../security/test.exe >server.log 2>&1 &
#   $ SERVER_PID=$!
#   $ trap 'stop_server $SERVER_PID' EXIT
#
stop_server ()
{
  stop_server_status=1
  if send_command "$1" "${2:-local.cmd}" shutdown; then
    stop_server_i=0
    while kill -0 "$1" 2>/dev/null && [ "$stop_server_i" -lt 200 ]; do
      stop_server_i=$((stop_server_i + 1))
      sleep 0.05
    done
    stop_server_status=0
  fi
  if kill -0 "$1" 2>/dev/null; then
    echo "stop_server: the server does not stop, killing it" >&2
    kill "$1" 2>/dev/null
    stop_server_status=1
  fi
  # Reap the server when it is a child of this shell
  wait "$1" 2>/dev/null
  return "$stop_server_status"
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
