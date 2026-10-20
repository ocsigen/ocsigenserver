Conformance of responses: HEAD, byte ranges, conditional requests and
compression (RFC 9110 sections 8.4, 9.3.2, 12.5.3, 13 and 14). Answers marked
BUG show the current behaviour, with the expected one in the text.

The modification time of the files is fixed, so that their validators are
known: ETag W/"5e0c89c0-6" for hello.txt ("hello\n"), W/"5e0c89c0-4000" for
compressed/big.txt (16 KiB), and Last-Modified Wed, 01 Jan 2020 12:00:00 GMT.
The security headers added to every response, and "connection: keep-alive",
are not shown.

  $ . ./fixture/helpers.sh
  $ start_server 8473
  $ TZ=UTC touch -t 202001011200 www/public/hello.txt www/compressed/big.txt
  $ req () {
  >   method=$1 path=$2; shift 2
  >   { printf '%s %s HTTP/1.1\r\nHost: x\r\n' "$method" "$path"
  >     for header in "$@"; do printf '%s\r\n' "$header"; done
  >     printf '\r\n'; } |
  >   rawhttp send --port 8473 --idle 0.5 --validators |
  >   grep -viE '^(server|connection|x-content-type-options|x-frame-options|referrer-policy):'; }
  $ status () { req "$@" | grep '^HTTP/'; }
  $ E='W/"5e0c89c0-6"'

  $ req GET /hello.txt
  HTTP/1.1 200 OK
  content-type: text/plain
  accept-ranges: bytes
  etag: W/"5e0c89c0-6"
  last-modified: Wed, 01 Jan 2020 12:00:00 GMT
  content-length: 6
  
  hello
  [idle]

HEAD (RFC 9110 section 9.3.2)
=============================

The answer to HEAD has the header section of the answer to GET, and no
content, whatever the status and the coding. Content there would be read as
the beginning of the next response on a persistent connection.

  $ req HEAD /hello.txt
  HTTP/1.1 200 OK
  content-type: text/plain
  accept-ranges: bytes
  etag: W/"5e0c89c0-6"
  last-modified: Wed, 01 Jan 2020 12:00:00 GMT
  content-length: 6
  
  [idle]
  $ req HEAD /nothere
  HTTP/1.1 404 Not Found
  content-type: text/plain; charset=utf-8
  content-length: 16
  
  [idle]
  $ req HEAD /compressed/big.txt 'Accept-Encoding: gzip' |
  >   grep -viE '^(content-type|accept-ranges|etag|last-modified):'
  HTTP/1.1 200 OK
  content-encoding: gzip
  vary: accept-encoding
  transfer-encoding: chunked
  
  [idle]
  $ { printf 'HEAD /hello.txt HTTP/1.1\r\nHost: x\r\n\r\n'
  >   printf 'GET /index.html HTTP/1.1\r\nHost: x\r\n\r\n'; } |
  >   rawhttp send --port 8473 --idle 0.5 | grep -E '^(HTTP/|hello|index|\[)'
  HTTP/1.1 200 OK
  HTTP/1.1 200 OK
  index
  [idle]

A 304 answer to HEAD has no content.

  $ req HEAD /hello.txt "If-None-Match: $E" | grep -E '^(HTTP/|\[)'
  HTTP/1.1 304 Not Modified
  [idle]

Byte ranges (RFC 9110 section 14)
=================================

  $ range () {
  >   req GET /hello.txt "Range: $1" |
  >     grep -iE '^(HTTP/|content-range|content-length)|^[a-z]{1,6}$'; }

Satisfiable ranges give 206 and the selected bytes. A last position past the
end is replaced by the last byte (section 14.1.2), and whitespace around the
range, which the grammar does not allow (section 14.1.1), is tolerated.

  $ range bytes=1-2
  HTTP/1.1 206 Partial Content
  content-range: bytes 1-2/6
  content-length: 2
  el
  $ range bytes=-2
  HTTP/1.1 206 Partial Content
  content-range: bytes 4-5/6
  content-length: 2
  o
  $ range bytes=2-
  HTTP/1.1 206 Partial Content
  content-range: bytes 2-5/6
  content-length: 4
  llo
  $ range bytes=0-0
  HTTP/1.1 206 Partial Content
  content-range: bytes 0-0/6
  content-length: 1
  h
  $ range bytes=5-100
  HTTP/1.1 206 Partial Content
  content-range: bytes 5-5/6
  content-length: 1
  $ range bytes=0-99999999999999999999
  HTTP/1.1 206 Partial Content
  content-range: bytes 0-5/6
  content-length: 6
  hello
  $ range 'bytes= 1-2 '
  HTTP/1.1 206 Partial Content
  content-range: bytes 1-2/6
  content-length: 2
  el

Unsatisfiable ranges give 416 with the length of the file (section 15.5.17).
An invalid range, with its last position before the first one (section
14.1.1), gets 416 too: a server may ignore or reject it (section 14.2).

  $ range bytes=6-
  HTTP/1.1 416 Requested Range Not Satisfiable
  content-range: bytes */6
  content-length: 0
  $ range bytes=-0
  HTTP/1.1 416 Requested Range Not Satisfiable
  content-range: bytes */6
  content-length: 0
  $ range bytes=3-1
  HTTP/1.1 416 Requested Range Not Satisfiable
  content-range: bytes */6
  content-length: 0

A server may ignore the Range header field (section 14.2). The whole file is
served for several ranges (only single ranges are supported), however many
there are, for an unknown unit, for invalid syntax, and for a first position
too large to be represented.

  $ range bytes=0-0,2-2
  HTTP/1.1 200 OK
  content-length: 6
  hello
  $ range "bytes=$(awk 'BEGIN { for (i = 0; i < 1000; i++) printf "0-0," }')0-0"
  HTTP/1.1 200 OK
  content-length: 6
  hello
  $ range items=1-2
  HTTP/1.1 200 OK
  content-length: 6
  hello
  $ range bytes=abc
  HTTP/1.1 200 OK
  content-length: 6
  hello
  $ range bytes=99999999999999999999-
  HTTP/1.1 200 OK
  content-length: 6
  hello

A missing file gives 404, not 416.

  $ status GET /nothere 'Range: bytes=1-2'
  HTTP/1.1 404 Not Found

BUG: range requests are only defined for GET, and the Range header field of
any other method must be ignored (section 14.2). HEAD gets 206. Expected:
200.

  $ status HEAD /hello.txt 'Range: bytes=1-2'
  HTTP/1.1 206 Partial Content

POST does not apply to a static file, Range or not.

  $ status POST /hello.txt 'Range: bytes=1-2' 'Content-Length: 0'
  HTTP/1.1 405 Method Not Allowed

Ranges of a file under deflatemod work as elsewhere.

  $ req GET /compressed/big.txt 'Range: bytes=0-9'
  HTTP/1.1 206 Partial Content
  content-type: text/plain
  accept-ranges: bytes
  etag: W/"5e0c89c0-4000"
  last-modified: Wed, 01 Jan 2020 12:00:00 GMT
  content-range: bytes 0-9/16384
  content-length: 10
  
  0123456789
  [idle]

A partial response is not compressed, even when the client accepts gzip: a
range applies to the selected representation (section 14.1), so compressing
the selected bytes would not match Content-Range.

  $ req GET /compressed/big.txt 'Range: bytes=0-9' 'Accept-Encoding: gzip'
  HTTP/1.1 206 Partial Content
  content-type: text/plain
  accept-ranges: bytes
  etag: W/"5e0c89c0-4000"
  last-modified: Wed, 01 Jan 2020 12:00:00 GMT
  content-range: bytes 0-9/16384
  content-length: 10
  
  0123456789
  [idle]

Conditional requests (RFC 9110 section 13)
==========================================

If-None-Match uses the weak comparison (section 13.1.2): a matching entity
tag, alone or in a list, or "*", gives 304. Entity tags are case-sensitive.

  $ status GET /hello.txt "If-None-Match: $E"
  HTTP/1.1 304 Not Modified
  $ status GET /hello.txt "If-None-Match: \"a\", $E"
  HTTP/1.1 304 Not Modified
  $ status GET /hello.txt "If-None-Match: \"a\",$E"
  HTTP/1.1 304 Not Modified
  $ status GET /hello.txt 'If-None-Match: *'
  HTTP/1.1 304 Not Modified
  $ status GET /hello.txt 'If-None-Match: "other"'
  HTTP/1.1 200 OK
  $ status GET /hello.txt 'If-None-Match: W/"5E0C89C0-6"'
  HTTP/1.1 200 OK

BUG: with the weak comparison, "5e0c89c0-6" matches W/"5e0c89c0-6" (section
8.8.3.2). Expected: 304.

  $ status GET /hello.txt 'If-None-Match: "5e0c89c0-6"'
  HTTP/1.1 200 OK

A 304 response carries the validators, and no Content-Length, which would
have to be the one of the 200 response (section 8.6).

  $ req GET /hello.txt "If-None-Match: $E"
  HTTP/1.1 304 Not Modified
  etag: W/"5e0c89c0-6"
  last-modified: Wed, 01 Jan 2020 12:00:00 GMT
  
  [idle]

BUG: a 304 response must carry the Vary header field that the 200 response
would have (section 15.4.5). The 304 response for a compressed file has
none. Expected: "vary: accept-encoding".

  $ req GET /compressed/big.txt 'If-None-Match: W/"5e0c89c0-4000"' \
  >   'Accept-Encoding: gzip'
  HTTP/1.1 304 Not Modified
  etag: W/"5e0c89c0-4000"
  last-modified: Wed, 01 Jan 2020 12:00:00 GMT
  
  [idle]

If-Modified-Since gives 304 when the file has not changed since the date, and
is ignored when the date is invalid, or when If-None-Match is present
(section 13.1.3).

  $ status GET /hello.txt 'If-Modified-Since: Wed, 01 Jan 2020 12:00:00 GMT'
  HTTP/1.1 304 Not Modified
  $ status GET /hello.txt 'If-Modified-Since: Wed, 01 Jan 2020 11:59:59 GMT'
  HTTP/1.1 200 OK
  $ status GET /hello.txt 'If-Modified-Since: garbage'
  HTTP/1.1 200 OK
  $ status GET /hello.txt 'If-Modified-Since: Wed, 01 Jan 2020 12:00:00 CET'
  HTTP/1.1 200 OK
  $ status GET /hello.txt 'If-None-Match: "other"' \
  >   'If-Modified-Since: Wed, 01 Jan 2020 12:00:00 GMT'
  HTTP/1.1 200 OK

Only a date equal to Last-Modified gives 304: a later one gives 200. RFC 9110
section 13.1.3 allows servers to honor only exact matches, although "not
modified since" is more useful.

  $ status GET /hello.txt 'If-Modified-Since: Wed, 01 Jan 2020 12:00:01 GMT'
  HTTP/1.1 200 OK

BUG: a recipient must accept the obsolete date formats, RFC 850 and asctime
(section 5.6.7). Expected: 304 for both requests.

  $ status GET /hello.txt 'If-Modified-Since: Wednesday, 01-Jan-20 12:00:00 GMT'
  HTTP/1.1 200 OK
  $ status GET /hello.txt 'If-Modified-Since: Wed Jan  1 12:00:00 2020'
  HTTP/1.1 200 OK

BUG: If-Match and If-Unmodified-Since are ignored. A false condition must be
answered 412 (sections 13.1.1, 13.1.4 and 13.2.2). If-Match uses the strong
comparison, which a weak entity tag never passes. Expected: 412 for the three
requests.

  $ status GET /hello.txt 'If-Match: "other"'
  HTTP/1.1 200 OK
  $ status GET /hello.txt "If-Match: $E"
  HTTP/1.1 200 OK
  $ status GET /hello.txt 'If-Unmodified-Since: Tue, 01 Jan 2019 12:00:00 GMT'
  HTTP/1.1 200 OK

Methods other than GET and HEAD do not apply to a static file: they get 405
whatever the preconditions, which are only evaluated for a successful answer
(section 13.2.1).

  $ status POST /hello.txt 'If-None-Match: *' 'Content-Length: 0'
  HTTP/1.1 405 Method Not Allowed
  $ status DELETE /hello.txt "If-None-Match: $E"
  HTTP/1.1 405 Method Not Allowed

Preconditions are not evaluated when the answer would not be a success
(section 13.2.1).

  $ status GET /nothere 'If-None-Match: *'
  HTTP/1.1 404 Not Found
  $ status GET /nothere 'If-Match: "other"'
  HTTP/1.1 404 Not Found

If-Range (section 13.1.5): the range is served when the validator matches
with the strong comparison, the whole file otherwise. A date must be exactly
the Last-Modified date.

  $ status GET /hello.txt 'Range: bytes=1-2' 'If-Range: "other"'
  HTTP/1.1 200 OK
  $ status GET /hello.txt 'Range: bytes=1-2' 'If-Range: "5e0c89c0-6"'
  HTTP/1.1 200 OK
  $ status GET /hello.txt 'Range: bytes=1-2' \
  >   'If-Range: Wed, 01 Jan 2020 12:00:00 GMT'
  HTTP/1.1 206 Partial Content
  $ status GET /hello.txt 'Range: bytes=1-2' \
  >   'If-Range: Wed, 01 Jan 2020 11:00:00 GMT'
  HTTP/1.1 200 OK

A weak entity tag never matches with the strong comparison, so it does not
select the range. The weak ETag only depends on the modification time (in
seconds) and the size of the file: a file rewritten within the same second
with the same size keeps it, and a resumed download would then mix two
versions of the file.

  $ status GET /hello.txt 'Range: bytes=1-2' "If-Range: $E"
  HTTP/1.1 200 OK

Compression (RFC 9110 sections 8.4 and 12.5.3)
==============================================

  $ coding () {
  >   req GET /compressed/big.txt "Accept-Encoding: $1" |
  >     grep -iE '^(HTTP/|content-encoding|vary|content-length|transfer-encoding)'; }

The coding is chosen with the q-values, zstd first when several are equally
acceptable. "x-gzip" is gzip (section 8.4.1.3). Whitespace and empty list
elements are allowed.

  $ coding gzip
  HTTP/1.1 200 OK
  content-encoding: gzip
  vary: accept-encoding
  transfer-encoding: chunked
  $ coding deflate
  HTTP/1.1 200 OK
  content-encoding: deflate
  vary: accept-encoding
  transfer-encoding: chunked
  $ coding zstd
  HTTP/1.1 200 OK
  content-encoding: zstd
  vary: accept-encoding
  transfer-encoding: chunked
  $ coding x-gzip | grep content-encoding
  content-encoding: gzip
  $ coding 'gzip;q=0.5, deflate' | grep content-encoding
  content-encoding: deflate
  $ coding 'gzip;q=0.5, deflate;q=0.4' | grep content-encoding
  content-encoding: gzip
  $ coding 'zstd;q=0.1, gzip;q=0.1' | grep content-encoding
  content-encoding: zstd
  $ coding 'gzip ; q=0.5 , deflate;q=0.4' | grep content-encoding
  content-encoding: gzip
  $ coding ', gzip' | grep content-encoding
  content-encoding: gzip

The content starts with the header of its format: gzip (RFC 1952), zlib for
deflate (RFC 1950), zstd (RFC 8878). Its decoding is checked in
test/extensions/deflatemod.t.

  $ for c in gzip deflate zstd; do
  >   req GET /compressed/big.txt "Accept-Encoding: $c" |
  >     sed -n '/^$/{n;n;p;q;}' | sed -E 's/^((\\x[0-9A-F]{2}|[^\\]){0,2}).*/\1/'
  > done
  \x1F\x8B
  x\x9C
  (\xB5

The file is not compressed when no supported coding is acceptable: unknown
codings, q=0, an invalid q-value. With "*", any coding may be used, including
none. When identity is excluded too, 406 is allowed (sections 12.4.1 and
12.5.3).

  $ coding br
  HTTP/1.1 200 OK
  content-length: 16384
  $ coding 'gzip;q=0'
  HTTP/1.1 200 OK
  content-length: 16384
  $ coding 'gzip;q=abc'
  HTTP/1.1 200 OK
  content-length: 16384
  $ coding 'gzip;q=0, *'
  HTTP/1.1 200 OK
  content-length: 16384
  $ coding '*'
  HTTP/1.1 200 OK
  content-length: 16384
  $ coding '*;q=0'
  HTTP/1.1 406 Not Acceptable
  content-length: 21
  $ coding 'identity;q=0'
  HTTP/1.1 406 Not Acceptable
  content-length: 21

BUG: content-coding names are case-insensitive (section 8.4.1). Expected:
gzip.

  $ coding GZIP
  HTTP/1.1 200 OK
  content-length: 16384

BUG: the response depends on Accept-Encoding, so it should carry
"Vary: accept-encoding" even when it is not compressed (section 12.5.5).
Otherwise a shared cache may serve the uncompressed copy to every client.

  $ coding ''
  HTTP/1.1 200 OK
  content-length: 16384
  $ req GET /compressed/big.txt | grep -ic '^vary'
  0
  [1]

A compressed response is well framed: a request that follows on the same
connection is answered.

  $ { printf 'GET /compressed/big.txt HTTP/1.1\r\nHost: x\r\nAccept-Encoding: gzip\r\n\r\n'
  >   printf 'GET /hello.txt HTTP/1.1\r\nHost: x\r\n\r\n'; } |
  >   rawhttp send --port 8473 --idle 0.5 --settle 0.5 --status
  HTTP/1.1 200 OK
  HTTP/1.1 200 OK
  [idle]

BUG: the chunked transfer coding must not be used in a response to an
HTTP/1.0 request (RFC 9112 section 6.1): an HTTP/1.0 client takes the chunk
sizes as part of the content, and the compressed data is corrupted. Expected:
no Transfer-Encoding, content delimited by closing the connection.

  $ printf 'GET /compressed/big.txt HTTP/1.0\r\nHost: x\r\nAccept-Encoding: gzip\r\n\r\n' |
  >   rawhttp send --port 8473 --idle 0.5 |
  >   grep -iE '^(HTTP/|transfer-encoding|content-length|0$|\[)'
  HTTP/1.1 200 OK
  transfer-encoding: chunked
  0
  [closed]

The server still works.

  $ alive 8473
  HTTP/1.1 200 OK
