The echo server of the differential tests (see test/differential/README.md)
answers each request with the request as Ocsigen Server understood it.
Two of them are started here, and compared with httpdiff.

  $ . ../server-test-helpers.sh
  $ free_ports 3
  $ mkdir -p a/log b/log
  $ (cd a && exec ../../http-echo/echo.exe "$PORT1") >a.log 2>&1 &
  $ A=$!
  $ (cd b && exec ../../http-echo/echo.exe "$PORT2") >b.log 2>&1 &
  $ B=$!
  $ trap 'stop_server $A a/local.cmd; stop_server $B b/local.cmd' EXIT
  $ wait_server $A a.log "http://127.0.0.1:$PORT1/"
  $ wait_server $B b.log "http://127.0.0.1:$PORT2/"
  $ cat > corpus.txt <<'CORPUS'
  > order-and-repeats "GET /a%20b?c HTTP/1.1\r\nHost: h\r\nZ: 1\r\nA: 2\r\nZ: 3\r\n\r\n"
  > body "POST / HTTP/1.1\r\nHost: h\r\nContent-Length: 2\r\n\r\nab"
  > chunked "POST / HTTP/1.1\r\nHost: h\r\nTransfer-Encoding: chunked\r\n\r\n2\r\nab\r\n0\r\n\r\n"
  > http-1.0 "GET / HTTP/1.0\r\n\r\n"
  > pipelined "GET /1 HTTP/1.1\r\nHost: h\r\n\r\nGET /2 HTTP/1.1\r\nHost: h\r\n\r\n"
  > two-hosts "GET / HTTP/1.1\r\nHost: h\r\nHost: i\r\n\r\n"
  > garbage "garbage\r\n\r\n"
  > CORPUS

The headers come in the order they were sent, with their repeats, and the
body without its framing. A rejected request gets the server's own answer.

  $ ../differential/httpdiff.exe show --settle 0.5 --idle 2 \
  >   --target "ocsigen=127.0.0.1:$PORT1" \
  >   corpus.txt
  == corpus/order-and-repeats
  -- ocsigen
  "GET" "/a%20b?c" "1.1" ["Host": "h"; "Z": "1"; "A": "2"; "Z": "3"] ""
  [idle]
  == corpus/body
  -- ocsigen
  "POST" "/" "1.1" ["Host": "h"; "Content-Length": "2"] "ab"
  [idle]
  == corpus/chunked
  -- ocsigen
  "POST" "/" "1.1" ["Host": "h"; "Transfer-Encoding": "chunked"] "ab"
  [idle]
  == corpus/http-1.0
  -- ocsigen
  "GET" "/" "1.0" [] ""
  [closed]
  == corpus/pipelined
  -- ocsigen
  "GET" "/1" "1.1" ["Host": "h"] ""
  "GET" "/2" "1.1" ["Host": "h"] ""
  [idle]
  == corpus/two-hosts
  -- ocsigen
  status 400
  [closed]
  == corpus/garbage
  -- ocsigen
  [idle]

Two servers that understand requests the same way agree on every case.

  $ ../differential/httpdiff.exe run --settle 0.5 --idle 2 \
  >   --target "a=127.0.0.1:$PORT1" \
  >   --target "b=127.0.0.1:$PORT2" corpus.txt
  == corpus/order-and-repeats: agree
  == corpus/body: agree
  == corpus/chunked: agree
  == corpus/http-1.0: agree
  == corpus/pipelined: agree
  == corpus/two-hosts: agree
  == corpus/garbage: agree
  7 cases, 0 with disagreements, 0 with failures

A server that cannot be asked is reported, and makes the run fail. Nothing
listens on the third port.

  $ ../differential/httpdiff.exe run --case corpus/body \
  >   --target "a=127.0.0.1:$PORT1" --target "down=127.0.0.1:$PORT3" corpus.txt
  == corpus/body: down: [failed: Connection refused]
  == corpus/body: not compared
  1 cases, 0 with disagreements, 1 with failures
  [1]
