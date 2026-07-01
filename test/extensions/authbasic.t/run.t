Authbasic protects a site with HTTP Basic authentication.

  $ . ../../server-test-helpers.sh
  $ printf 'protected' > index.html
  $ run_server ../authbasic_server.exe

Without credentials the server asks for authentication (401 + a challenge for
the configured realm).

  $ curl_ index.html
  HTTP/1.1 401 Unauthorized
  WWW-Authenticate: Basic realm="test"
  server: Ocsigen
  content-length: 61
  
  Error: Ocsigen.Ocsigen_cohttp.Ext_http_error(324098644, 0, _)

Wrong credentials are also rejected.

  $ curl_ index.html -u user:wrong | head -1
  HTTP/1.1 401 Unauthorized

The right credentials grant access.

  $ curl_ index.html -u user:pass
  HTTP/1.1 200 OK
  content-type: text/html
  server: Ocsigen
  content-length: 9
  
  protected
