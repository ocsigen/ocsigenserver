The stop function of a multipart part, which closes its file, is called once,
even when it fails, and the first error is the one reported. A failure of
stop after another error is logged.

  $ ../multipart/test.exe 2>&1
  add succeeds, stop succeeds -> ok, stop called 1 time(s)
  add succeeds, stop fails -> Stop_failed 1, stop called 1 time(s)
  add fails, stop succeeds -> Add_failed, stop called 1 time(s)
  test.exe: [WARNING] Error while closing a part, after another error: Stop_failed 1
  add fails, stop fails -> Add_failed, stop called 1 time(s)
