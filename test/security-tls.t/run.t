F5: the HTTPS server must require TLS 1.2 at minimum (TLS 1.0/1.1 deprecated).
POSIX sh.

  $ . ../server-test-helpers.sh
  $ free_ports 1
  $ mkdir -p log
  $ openssl req -x509 -newkey rsa:2048 -nodes -keyout privkey.pem \
  >   -out cert.pem -days 2 -subj /CN=localhost >/dev/null 2>&1
  $ ../security-tls/test.exe $PORT1 >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'stop_server $SERVER_PID' EXIT
  $ wait_server $SERVER_PID server.log local.cmd https://127.0.0.1:$PORT1/

A TLS 1.2 client succeeds:

  $ curl -sk --tlsv1.2 --tls-max 1.2 -o /dev/null -w '%{http_code}\n' \
  >   https://127.0.0.1:$PORT1/
  200

A TLS 1.1 client is refused (curl returns a non-zero exit code). With OpenSSL
3, the default security level already rules out TLS 1.1 on both sides, which
would make this check pass even without the minimum version: the server (with
<ciphers>) and the client both lower it to 0, so that only the minimum version
refuses the connection.

  $ curl -sk --tls-max 1.1 --ciphers 'DEFAULT:@SECLEVEL=0' \
  >   https://127.0.0.1:$PORT1/ >/dev/null 2>&1 \
  >   && echo accepted || echo rejected
  rejected
