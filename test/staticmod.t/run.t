Staticmod with a configuration file.

  $ . ../server-test-helpers.sh
  $ mkdir -p www/.git www/.well-known www/sub open
  $ echo index > www/index.html
  $ echo secret > www/.env; echo secret > www/.git/config
  $ echo secret > www/sub/.htpasswd; echo visible > www/sub/file
  $ echo contact > www/.well-known/security.txt
  $ echo secret > www/.well-known/.hidden
  $ echo error page > error.html
  $ echo secret > open/.env
  $ cat > site.conf <<'CONF'
  > <ocsigen><server>
  > <port>8085</port>
  > <extension findlib-package="ocsigenserver.ext.staticmod"/>
  > <extension findlib-package="ocsigenserver.ext.extendconfiguration"/>
  > <host hostfilter="*">
  >   <listdirs value="true"/>
  >   <static regexp="~([^/]*)(.*)" dest="/nonexistent/\1\2" />
  >   <site path="open"><static dir="open" hidedotfiles="false" /></site>
  >   <static dir="www" />
  >   <static code="404" dest="error.html" />
  > </host>
  > </server></ocsigen>
  > CONF
  $ ocsigenserver -c site.conf >server.log 2>&1 &
  $ SERVER_PID=$!
  $ trap 'kill $SERVER_PID 2>/dev/null' EXIT
  $ wait_http http://127.0.0.1:8085/index.html

A <static regexp> that does not match the path lets the next one serve it:

  $ curl -s -w ' %{http_code}\n' --max-time 10 http://127.0.0.1:8085/index.html
  index
   200

The paths with a segment that starts with a dot get a 404, wherever the dot
is, also when it is percent-encoded or when the path is a directory.
/.well-known/ (RFC 8615) is served, but not the dotfiles inside it:

  $ for p in .env %2Eenv .git/ .git/config sub/.htpasswd sub%2F.htpasswd \
  >   .well-known/security.txt .well-known/.hidden sub/file; do
  >   curl -s -o /dev/null -w "$p %{http_code}\n" --max-time 10 \
  >     http://127.0.0.1:8085/$p
  > done
  .env 404
  %2Eenv 404
  .git/ 404
  .git/config 404
  sub/.htpasswd 404
  sub%2F.htpasswd 404
  .well-known/security.txt 200
  .well-known/.hidden 404
  sub/file 200

This 404 is the one of a missing file: the error page is served.

  $ curl -s --max-time 10 http://127.0.0.1:8085/.env
  error page

The preconditions and the range of the request do not apply to an error
page (RFC 9110 sections 13.2.1 and 14.2): it is sent whole, with its status.

  $ for h in 'If-None-Match: *' 'If-Match: "x"' 'Range: bytes=0-1'; do
  >   curl -s -w ' %{http_code}\n' --max-time 10 -H "$h" \
  >     http://127.0.0.1:8085/missing
  > done
  error page
   404
  error page
   404
  error page
   404

Directory listings leave the hidden files out, except /.well-known/:

  $ rm www/index.html
  $ curl -s --max-time 10 http://127.0.0.1:8085/ | grep -o '<li>[^/]*</a>'
  <li><a href=".well-known">.well-known</a>
  <li><a href="sub">sub</a>
  $ curl -s --max-time 10 http://127.0.0.1:8085/sub/ | grep -o '<li>[^/]*</a>'
  <li><a href="file">file</a>
  $ curl -s --max-time 10 http://127.0.0.1:8085/.well-known/ \
  >   | grep -o '<li>[^/]*</a>'
  <li><a href="security.txt">security.txt</a>

With hidedotfiles="false", dotfiles are served and listed:

  $ curl -s --max-time 10 http://127.0.0.1:8085/open/.env
  secret
  $ curl -s --max-time 10 http://127.0.0.1:8085/open/ | grep -o '<li>[^/]*</a>'
  <li><a href=".env">.env</a>

A value other than true or false is refused:

  $ sed 's/hidedotfiles="false"/hidedotfiles="maybe"/' site.conf > bad.conf
  $ ocsigenserver -c bad.conf >bad.log 2>&1; echo "exit $?"
  exit 50
  $ grep -o 'Bad value "maybe" in <static hidedotfiles=.*' bad.log
  Bad value "maybe" in <static hidedotfiles="..." />: it should be true or false
