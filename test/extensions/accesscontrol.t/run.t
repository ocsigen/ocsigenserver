Accesscontrol forbids access to paths matching a condition.

  $ . ../../server-test-helpers.sh
  $ printf 'public data' > public.txt
  $ printf 'classified' > secret.txt
  $ run_server ../accesscontrol_server.exe

A path matching the "secret" condition is forbidden.

  $ curl_ secret.txt | head -1
  HTTP/1.1 403 Forbidden

Any other path is served normally.

  $ curl_ public.txt
  HTTP/1.1 200 OK
  content-type: text/plain
  server: Ocsigen
  content-length: 11
  
  public data
