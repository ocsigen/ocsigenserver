Request paths are percent-decoded, so that a file whose name is not made of
unreserved characters can be reached, and so that an encoded ".." cannot be
used to escape the served directory.

  $ mkdir www
  $ printf 'accented\n' > "www/café.txt"
  $ printf 'secret\n' > secret.txt

  $ ocsigenserver --serve www --port 8062 >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT

Warm the server up, and check that a plain request works.

  $ curl -s --retry 20 --retry-delay 1 --retry-connrefused \
  >   -o /dev/null -w "%{http_code}\n" http://127.0.0.1:8062/
  404

A percent-encoded file name is decoded before the file is looked up.

  $ curl -s http://127.0.0.1:8062/caf%C3%A9.txt
  accented

Neither a literal nor an encoded ".." reaches outside the served directory,
whether the slashes are encoded or not.

  $ for p in "/../secret.txt" "/..%2fsecret.txt" "/%2e%2e%2fsecret.txt" \
  >          "/%2e%2e/secret.txt" "/..%2F..%2Fsecret.txt"; do
  >   curl -s --path-as-is -o /dev/null -w "%{http_code} $p\n" \
  >     "http://127.0.0.1:8062$p"
  > done
  404 /../secret.txt
  404 /..%2fsecret.txt
  404 /%2e%2e%2fsecret.txt
  404 /%2e%2e/secret.txt
  404 /..%2F..%2Fsecret.txt
