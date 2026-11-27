# Differential tests of HTTP/1.1 parsers

`httpdiff` sends the same bytes to several HTTP servers and compares how they
understood them. A server that ends a request at another place than the
others, or accepts what they reject, is worth a look: behind a proxy that
parses differently, such a difference can let a request reach a server
unchecked (request smuggling, vhost confusion).

Each server must be an *echo server*: it answers each request it accepts with
a 200 response whose body is the request as it understood it, as a JSON object
in the format of [HTTP Garden](https://github.com/narfindustries/http-garden)
(see `echo.mli`). The echo server of Ocsigen Server is
`../http-echo/echo.ml`. In HTTP Garden, the services whose `x-props` have
`role: origin` are echo servers (about 35); the others are proxies.

A response to HEAD has no body, so it cannot carry an echo: the corpus has no
HEAD request.

## Corpus

The files of `corpus/` hold the inputs, one per line: a name, then the bytes
as an OCaml string literal (see `corpus.mli`). A case is named after its file
and its name, such as `host/ipv6`. An input may hold several requests, so
that the place where each server ends the first one shows.

## Usage

```
httpdiff show --target NAME=[tls:]HOST:PORT ... FILE...
httpdiff run --target NAME=[tls:]HOST:PORT ... FILE...
```

`show` prints what each server answered. `run` groups the servers that agree,
case by case, and prints the groups of the cases where they do not; it exits
with status 1 when a server could not be asked a case. Options:

- `--case FILE/NAME`: only this case (repeatable);
- `--first-byte SECONDS`: how long to wait for the first byte of an answer
  (default 2);
- `--idle SECONDS`: how long to wait for more (default 0.5);
- `--settle SECONDS`: the same, after complete responses that keep the
  connection open (default: as `--idle`). A short delay makes the runs
  faster, but a server that is slow to answer a second request may then be
  taken for one that does not answer it;
- `--underscore-names NAME`: the server `NAME` writes the hyphens of header
  names as underscores, as WSGI and CGI do (gunicorn, for instance).;
- `--ignore-field FIELD`: compare the requests without this header field,
  which some server adds or removes itself (the `added-headers` and
  `removed-headers` of `quirks.yml` in HTTP Garden: nginx adds an empty
  Content-Type, for instance).

The comparison ignores what is not a difference of understanding: the case
of header names, the order of fields of different names, whether fields of
the same name are joined in one, Content-Length and Transfer-Encoding (the
body is compared instead), and the status code of a rejection. An
absolute-form target counts as its path, with its authority as the host
name, as RFC 9112 asks; servers that report it as received and those that
report what they made of it can then be compared. A server that rejects a request with 400 and one that closes the
connection agree. So do a server that refuses the end of an input and one
that waits for more bytes, taking it for an unfinished body: `show` tells
them apart.

## Running a campaign against HTTP Garden

Build the two programs and start the echo server of Ocsigen Server, in a
directory of its own (it writes `log/` and the command pipe `local.cmd`):

```
dune build test/http-echo/echo.exe test/differential/httpdiff.exe
mkdir -p $DIR/log && cd $DIR && $REPO/_build/default/test/http-echo/echo.exe 8080 &
```

Then build and start the servers to compare with, from the images of HTTP
Garden. Its own tools are Python programs: do not run them from its
directory. Only Docker is needed here, run from outside the clone. The
`chmod` makes the files that the images copy readable by their servers,
whatever the umask of the clone:

```
git clone https://github.com/narfindustries/http-garden $GARDEN
chmod -R a+rX $GARDEN
docker build -t http-garden-soil $GARDEN/images/http-garden-soil
docker compose -p http-garden -f $GARDEN/docker-compose.yml build nginx apache_httpd
```

Without the `compose` plugin of Docker, build each image with `docker build`
and the `args` and `context` of its service in `docker-compose.yml`:

```
docker build -t http-garden-nginx --build-arg APP_REPO=... \
  --build-arg APP_BRANCH=... --build-arg APP_VERSION=... $GARDEN/images/nginx
```

Start each server, and get its port on the loopback interface:

```
docker run -d --rm --name garden-nginx -p 127.0.0.1::80 http-garden-nginx
docker port garden-nginx 80/tcp
```

The servers marked `requires-tls` in `docker-compose.yml` listen on port 443
and are given as `--target NAME=tls:127.0.0.1:PORT`. Then:

```
_build/default/test/differential/httpdiff.exe run \
  --target ocsigen=127.0.0.1:8080 \
  --target nginx=127.0.0.1:PORT1 --target apache=127.0.0.1:PORT2 \
  test/differential/corpus/*.txt
```

Stop the servers with `docker stop garden-nginx ...`. Some servers differ in
ways that are known and benign (see `quirks.yml` in HTTP Garden), for
instance by removing or joining some headers.

Do not publish the results of a campaign that shows an unfixed security bug:
report it first.
