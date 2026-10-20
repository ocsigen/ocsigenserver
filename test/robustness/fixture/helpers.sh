# POSIX shell functions for the robustness tests. Kept free of bashisms: dune
# runs cram tests with /bin/sh, which is dash on Debian and Ubuntu.
#
# The server is started from fixture/server.conf.in, with this file tree
# (relative to the test directory):
#
#   www/public/index.html         "index"
#   www/public/hello.txt          "hello"
#   www/public/private/x.txt      "private" (forbidden by accesscontrol)
#   www/public/backend/hello.txt  "backend" (target of proxy.test)
#   www/public/fixture-id.txt     identifies the server of this test
#   www/compressed/big.txt        16 KiB of text (compressed with gzip)
#   www/admin/index.html          "admin" (host admin.test, password)
#   www/secret.txt                "SECRET" (outside every served directory)
#   outside.txt                   "OUTSIDE" (outside www)
#
# Every cram test uses its own ports so that tests can run in parallel.

FIXTURE=fixture

make_tree ()
{
  mkdir -p log data www/public/private www/public/backend www/compressed \
    www/admin
  printf 'index\n' > www/public/index.html
  printf 'hello\n' > www/public/hello.txt
  printf 'private\n' > www/public/private/x.txt
  printf 'backend\n' > www/public/backend/hello.txt
  printf '%s\n' "$PWD" > www/public/fixture-id.txt
  awk 'BEGIN { for (i = 0; i < 1024; i++) print "0123456789abcde" }' \
    > www/compressed/big.txt
  printf 'admin\n' > www/admin/index.html
  printf 'SECRET\n' > www/secret.txt
  printf 'OUTSIDE\n' > outside.txt
}

# Self-signed certificate for the TLS tests.
make_certificate ()
{
  command -v openssl >/dev/null ||
    { echo "make_certificate: the openssl command is needed"; return 1; }
  openssl req -x509 -newkey ec -pkeyopt ec_paramgen_curve:prime256v1 \
    -nodes -keyout privkey.pem -out cert.pem -days 2 \
    -subj /CN=localhost > openssl.log 2>&1 || { cat openssl.log; return 1; }
}

# start_server PORT [TLS_PORT [SERVER_OPTIONS]]
#
# Starts the server on PORT (plain HTTP) and, when TLS_PORT is given and not
# empty, on TLS_PORT (HTTPS, with the certificate of make_certificate).
# SERVER_OPTIONS is raw XML added inside <server>. The server is stopped when
# the test ends. Its log is in server.log.
start_server ()
{
  port=$1
  ports="<port>127.0.0.1:$port</port>"
  if [ -n "$2" ]; then
    ports="$ports<port protocol=\"HTTPS\">127.0.0.1:$2</port>"
    ports="$ports<ssl><certificate>$PWD/cert.pem</certificate>"
    ports="$ports<privatekey>$PWD/privkey.pem</privatekey></ssl>"
  fi
  make_tree
  sed -e "s|@PORTS@|$ports|" -e "s|@SERVER_OPTIONS@|$3|" \
    -e "s|@PORT@|$port|g" -e "s|@ROOT@|$PWD|g" \
    "$FIXTURE/server.conf.in" > server.conf
  ocsigenserver -c server.conf > server.log 2>&1 &
  SERVER_PID=$!
  trap 'stop_server' EXIT
  # Wait for the server for up to 20 s, half a second at a time, and stop
  # waiting as soon as it has exited.
  tries=0
  until rawhttp wait --port "$port" --idle 0.5 > /dev/null 2>&1; do
    tries=$((tries + 1))
    if ! kill -0 "$SERVER_PID" 2>/dev/null || [ "$tries" -ge 40 ]; then
      echo "start_server: no server on port $port"; cat server.log; return 1
    fi
  done
  # The answer must come from this server, not from another process that
  # already listens on the port.
  printf 'GET /fixture-id.txt HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n' |
    rawhttp send --port "$port" --idle 2 | grep -qxF "$PWD" ||
    { echo "start_server: port $port is used by another process"
      cat server.log; return 1; }
}

stop_server ()
{
  kill "$SERVER_PID" 2>/dev/null
  wait "$SERVER_PID" 2>/dev/null
}

# alive PORT: prints the status line of the answer to a simple request, to
# check that the server still works.
alive ()
{
  printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n' |
    rawhttp send --port "$1" --status --idle 0.5 | grep "^HTTP/"
}
