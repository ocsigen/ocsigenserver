F5: the HTTPS server must require TLS 1.2 at minimum (TLS 1.0/1.1 deprecated).
Self-contained POSIX sh.

  $ mkdir -p log data
  $ openssl req -x509 -newkey rsa:2048 -nodes -keyout privkey.pem \
  >   -out cert.pem -days 2 -subj /CN=localhost >/dev/null 2>&1
  $ dune build ./test.exe 2>&1
  $ dune exec -- ./test.exe >server.log 2>&1 &
  $ trap 'echo shutdown > local.cmd 2>/dev/null; wait' EXIT
  $ i=0; while [ ! -e local.cmd ]; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done
  $ i=0; while ! curl -sk --tls-max 1.2 https://127.0.0.1:8453/ >/dev/null 2>&1; do
  >   i=$((i+1)); [ $i -gt 200 ] && break; sleep 0.05; done

A TLS 1.2 client succeeds:

  $ curl -sk --tlsv1.2 --tls-max 1.2 -o /dev/null -w '%{http_code}\n' https://127.0.0.1:8453/
  200

A TLS 1.1 client is refused (curl returns a non-zero exit code):

  $ curl -sk --tls-max 1.1 https://127.0.0.1:8453/ >/dev/null 2>&1 && echo accepted || echo rejected
  rejected
