The stop function of a multipart part, which closes its file, is called once,
even when it fails, and the first error is the one reported. A failure of
stop after another error is logged. A malformed body (a part header that is
not a header, a boundary without line end, no boundary at all, no closing
boundary) is a bad request, and the reason is logged.

  $ ../multipart/test.exe 2>&1
  add succeeds, stop succeeds -> ok, stop called 1 time(s)
  add succeeds, stop fails -> Stop_failed 1, stop called 1 time(s)
  add fails, stop succeeds -> Add_failed, stop called 1 time(s)
  test.exe: [WARNING] Error while closing a part, after another error: Stop_failed 1
  add fails, stop fails -> Add_failed, stop called 1 time(s)
  test.exe: [INFO] Multipart body rejected: a part has a malformed header
  "--B\r\nfoo\r\n\r\nx\r\n--B--\r\n" -> Bad request
  test.exe: [INFO] Multipart body rejected: a part has a malformed header
  "--B\r\n Content-Disposition: form-data\r\n\r\nx\r\n--B--\r\n" -> Bad request
  test.exe: [INFO] Multipart body rejected: the body or one of its parts ends too early
  "--B" -> Bad request
  test.exe: [INFO] Multipart body rejected: the body or one of its parts ends too early
  "--Bx" -> Bad request
  test.exe: [INFO] Multipart body rejected: the body or one of its parts ends too early
  "no boundary\r\n" -> Bad request
  test.exe: [INFO] Multipart body rejected: the body or one of its parts ends too early
  "--B\r\nContent-Disposition: form-data; name=\"f\"\r\n\r\nx" -> Bad request
