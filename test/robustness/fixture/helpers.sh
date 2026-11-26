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
# The server listens on free TCP ports, in PORT and TLS_PORT, since several
# test runs may happen at once on one machine (see free_ports in
# ../server-test-helpers.sh). A test writes no port number literally.

. ../server-test-helpers.sh

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

# start_server [--tls] [SERVER_OPTIONS]
#
# Starts the server on a free port, PORT (plain HTTP) and, with --tls, on a
# second one, TLS_PORT (HTTPS, with the certificate of make_certificate).
# SERVER_OPTIONS is raw XML added inside <server>. The server is stopped when
# the test ends. Its log is in server.log.
start_server ()
{
  fixture_tls=false
  if [ "${1-}" = --tls ]; then fixture_tls=true; shift; fi
  if $fixture_tls; then free_ports 2; else free_ports 1; fi || return 1
  PORT=$PORT1
  fixture_ports="<port>127.0.0.1:$PORT</port>"
  fixture_urls="http://127.0.0.1:$PORT/fixture-id.txt"
  if $fixture_tls; then
    TLS_PORT=$PORT2
    fixture_ports="$fixture_ports<port protocol=\"HTTPS\">"
    fixture_ports="${fixture_ports}127.0.0.1:$TLS_PORT</port>"
    fixture_ports="$fixture_ports<ssl><certificate>$PWD/cert.pem</certificate>"
    fixture_ports="$fixture_ports<privatekey>$PWD/privkey.pem</privatekey>"
    fixture_ports="$fixture_ports</ssl>"
    fixture_urls="$fixture_urls https://127.0.0.1:$TLS_PORT/fixture-id.txt"
  fi
  make_tree
  sed -e "s|@PORTS@|$fixture_ports|" -e "s|@SERVER_OPTIONS@|${1-}|" \
    -e "s|@PORT@|$PORT|g" -e "s|@ROOT@|$PWD|g" \
    "$FIXTURE/server.conf.in" > server.conf
  ocsigenserver -c server.conf > server.log 2>&1 &
  SERVER_PID=$!
  trap 'stop_server "$SERVER_PID"' EXIT
  # $fixture_urls is split into its URLs on purpose.
  wait_server "$SERVER_PID" server.log $fixture_urls || return 1
  # wait_server only checks that the server still runs once the URLs answer:
  # the answers must also come from this server, not from another process
  # that already listened on one of the ports.
  fixture_id "$PORT" || return 1
  if $fixture_tls; then fixture_id "$TLS_PORT" --tls || return 1; fi
}

# fixture_id PORT [--tls]: checks that the server on PORT is the one of this
# test, which serves fixture-id.txt with the test directory as content.
fixture_id ()
{
  printf 'GET /fixture-id.txt HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n' |
    rawhttp send --port "$1" ${2+"$2"} --idle 2 | grep -qxF "$PWD" ||
    { echo "start_server: port $1 is used by another process"
      cat server.log; return 1; }
}

# alive: prints the status line of the answer to a simple request on PORT,
# to check that the server still works.
alive ()
{
  printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n' |
    rawhttp send --port "$PORT" --status --idle 0.5 | grep "^HTTP/"
}
