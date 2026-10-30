The fields and files of valid multipart bodies arrive intact. The server
answers with one line per parameter, in order (its name and value, or the
size and digest of a value longer than 64 bytes), then one line per file
(its field name, file name, Content-Type, size and the digest of its
content). POSIX sh.

  $ . ../server-test-helpers.sh
  $ mkdir -p log uploads
  $ ../multipart-decoding/server.exe >server.log 2>&1 &
  $ SERVER_PID=$!
  $ wait_server $SERVER_PID server.log local.sock local.cmd
  $ post () { curl --unix-socket local.sock -s -f "$@" http://x/; }

The test files: a text file, an empty file, all the byte values, a binary
file built from them, a text with CRLF line ends, and a long text:

  $ echo hello > a.txt
  $ : > empty
  $ i=0; while [ $i -lt 256 ]; do
  >   printf %b "\\0$(printf %o $i)"; i=$((i+1)); done > bytes.bin
  $ for i in 1 2 3 4; do cat bytes.bin; done > 1k
  $ for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17; do
  >   cat 1k 1k 1k 1k; done > big.bin
  $ printf 'a\r\nb\r\n\r\n' > crlf.txt
  $ i=0; while [ $i -lt 7000 ]; do
  >   printf 'line %d\r\n' $i; i=$((i+1)); done > long.txt
  $ for f in bytes.bin big.bin long.txt; do
  >   echo $f $(wc -c < $f); done
  bytes.bin 256
  big.bin 69632
  long.txt 75890

Text fields and files, in a mixed order, some with the same name:

  $ post -F a=1 -F f=@a.txt -F b=2 -F f=@bytes.bin -F a=3 \
  >   -F 'g=@big.bin;type=image/png'
  param "a" "1"
  param "b" "2"
  param "a" "3"
  file "f" "a.txt" text/plain 6 bytes b1946ac92492d2347c6235b4d2611184
  file "f" "bytes.bin" application/octet-stream 256 bytes e2c865db4162bed963bfaa9ef6ac18f0
  file "g" "big.bin" image/png 69632 bytes 43668c66da6e85b2a221e536e3ade8af

An empty file, an empty value, a file and a value with CRLF inside, and a
long value:

  $ post -F e=@empty -F t= -F h=@crlf.txt -F 'c=<crlf.txt' -F 'l=<long.txt'
  param "t" ""
  param "c" "a\r\nb\r\n\r\n"
  param "l" 75890 bytes b00db3eca2fc0bbbeeea414231e127bc
  file "e" "empty" application/octet-stream 0 bytes d41d8cd98f00b204e9800998ecf8427e
  file "h" "crlf.txt" text/plain 8 bytes 13dee0d1bd96b11db585c462973cf462

Many parts: 120 parts, every sixth one a file. The answer is compared with
the expected one:

  $ i=0; set --; while [ $i -lt 120 ]; do i=$((i+1));
  >   if [ $((i % 6)) = 0 ]; then set -- "$@" -F "file$i=@a.txt"
  >   else set -- "$@" -F "f$i=v$i"; fi; done
  $ i=0; while [ $i -lt 120 ]; do i=$((i+1));
  >   [ $((i % 6)) = 0 ] || echo "param \"f$i\" \"v$i\""; done > expected
  $ i=0; while [ $i -lt 120 ]; do i=$((i+1)); [ $((i % 6)) = 0 ] &&
  >   echo "file \"file$i\" \"a.txt\" text/plain 6 bytes b1946ac92492d2347c6235b4d2611184"
  > done >> expected
  $ post "$@" | diff expected -

A non-ASCII field name and file name (UTF-8, printed escaped):

  $ post -F 'été=x' -F 'f=@a.txt;filename=été.txt'
  param "\195\169t\195\169" "x"
  file "f" "\195\169t\195\169.txt" text/plain 6 bytes b1946ac92492d2347c6235b4d2611184

The bodies below are sent raw. check sends the body in the file given as
first argument, with the Content-Type given as second argument, if any. It
prints the answer, or the status if it is not 200. Then chunks.exe decodes the
same body as a whole, cut in two at each offset, and in chunks of 1, 2, 3, 7,
64 and 4096 bytes. Its output is printed only if it differs from the answer.

  $ check () {
  >   ct=${2:-multipart/form-data; boundary=B}
  >   code=$(curl --unix-socket local.sock -s -o answer -w '%{http_code}' \
  >     --data-binary @"$1" -H "Content-Type: $ct" http://x/)
  >   if [ "$code" = 200 ]; then cat answer; else echo "status $code"; fi
  >   ../multipart-decoding/chunks.exe "$ct" "$1" > chunks
  >   [ "$code" = 200 ] && cmp -s answer chunks ||
  >     { echo "chunks.exe:"; cat chunks; }; }
  $ part () {
  >   printf '%s\r\nContent-Disposition: form-data; %s\r\n\r\n%b\r\n' \
  >     "--${3:-B}" "$1" "$2"; }

A simple body:

  $ { part 'name="a"' 1; part 'name="f"; filename="a.txt"' 'hello\n';
  >   printf %s '--B--'; } > simple
  $ check simple
  param "a" "1"
  file "f" "a.txt" - 6 bytes b1946ac92492d2347c6235b4d2611184

A UTF-8 file name, as browsers send it. Then a double quote in a file name:
browsers and curl 7.81 and later percent-encode it, and the server does not
decode it; older curl and curl --form-escape escape it with a backslash. Both
forms are sent raw, so that the test does not depend on the version of curl.
Then file names with backslashes, as browsers send a Windows path (Internet
Explorer, Edge in IE mode): a backslash is kept, except before a double quote
or another backslash, as \" and \\ are read as escapes for the clients that
send them. The cost is that the double backslash of a network path, which
browsers do not escape, is read as one. Last, a name and a file name that
end with a backslash, as browsers send them: the parameters are then read
again with every backslash kept:

  $ { part 'name="f"; filename="été.txt"' x
  >   part 'name="f"; filename="a%22b.txt"' x
  >   part 'name="f"; filename="a\"b.txt"' x
  >   part 'name="f"; filename="C:\dir\a.txt"' x
  >   part 'name="f"; filename="\\server\share\a.txt"' x
  >   part 'name="a\"' x
  >   part 'name="f"; filename="C:\dir\"' x
  >   printf %s '--B--'; } > names
  $ check names
  param "a\\" "x"
  file "f" "\195\169t\195\169.txt" - 1 bytes 9dd4e461268c8034f5c8564e155c67a6
  file "f" "a%22b.txt" - 1 bytes 9dd4e461268c8034f5c8564e155c67a6
  file "f" "a\"b.txt" - 1 bytes 9dd4e461268c8034f5c8564e155c67a6
  file "f" "C:\\dir\\a.txt" - 1 bytes 9dd4e461268c8034f5c8564e155c67a6
  file "f" "\\server\\share\\a.txt" - 1 bytes 9dd4e461268c8034f5c8564e155c67a6
  file "f" "C:\\dir\\" - 1 bytes 9dd4e461268c8034f5c8564e155c67a6

All the byte values in a file, and in a long file and a long value:

  $ { printf '%b' '--B\r\nContent-Disposition: form-data; name="f"; filename="b"\r\n\r\n'
  >   cat bytes.bin; printf '%b' '\r\n--B--\r\n'; } > binary
  $ check binary
  file "f" "b" - 256 bytes e2c865db4162bed963bfaa9ef6ac18f0
  $ { printf '%b' '--B\r\nContent-Disposition: form-data; name="f"; filename="b"\r\n\r\n'
  >   cat big.bin; printf '%b' '\r\n--B\r\nContent-Disposition: form-data; name="l"\r\n\r\n'
  >   cat long.txt; printf '%b' '\r\n--B--\r\n'; } > big
  $ check big
  param "l" 75890 bytes b00db3eca2fc0bbbeeea414231e127bc
  file "f" "b" - 69632 bytes 43668c66da6e85b2a221e536e3ade8af

Contents that look like a boundary but are not one: "--B" inside a line,
"\n--B" without CR, a CRLF followed by a prefix of the delimiter, a value
ending with CR or CRLF:

  $ { part 'name="a"' 'x--B--y\n--B\r--B\r'
  >   printf %s '--B--'; } > lookalike
  $ check lookalike
  param "a" "x--B--y\n--B\r--B\r"
  $ { part 'name="a"' '\r\n--BOUNDAR\r\n-\r\n--\r\n' BOUNDARY
  >   printf %s '--BOUNDARY--'; } > lookalike2
  $ check lookalike2 'multipart/form-data; boundary=BOUNDARY'
  param "a" "\r\n--BOUNDAR\r\n-\r\n--\r\n"

An empty file, an empty value, a value with CRLF inside:

  $ { part 'name="e"; filename="e"' ''; part 'name="t"' ''
  >   part 'name="c"' 'a\r\nb\r\n\r\n'; printf %s '--B--'; } > empty-parts
  $ check empty-parts
  param "t" ""
  param "c" "a\r\nb\r\n\r\n"
  file "e" "e" - 0 bytes d41d8cd98f00b204e9800998ecf8427e

Many parts, compared with the expected answer as above:

  $ i=0; while [ $i -lt 120 ]; do i=$((i+1));
  >   if [ $((i % 6)) = 0 ]; then
  >     printf '%s\r\nContent-Disposition: form-data; name="file%d"; filename="a.txt"\r\nContent-Type: text/plain\r\n\r\nhello\n\r\n' --B $i
  >   else part "name=\"f$i\"" "v$i"; fi; done > many
  $ printf %s '--B--' >> many
  $ check many | diff expected -

LF-only line ends:

  $ printf %b '--B\nContent-Disposition: form-data; name="a"\n\n1\n--B\nContent-Disposition: form-data; name="f"; filename="a.txt"\n\nx\r\ny\n--B--\n' > lf
  $ check lf
  param "a" "1"
  file "f" "a.txt" - 4 bytes e65975bbb212f40b0b9224d0ed17136d

A preamble before the first boundary, an epilogue after the closing one,
spaces after the boundaries, no CRLF after the closing boundary:

  $ printf %b 'preamble\r\n--B\r\nContent-Disposition: form-data; name="a"\r\n\r\n1\r\n--B--\r\nepilogue\r\n--B\r\nContent-Disposition: form-data; name="b"\r\n\r\n2\r\n' > epilogue
  $ check epilogue
  param "a" "1"
  $ printf %b '--B \t\r\nContent-Disposition: form-data; name="a"\r\n\r\n1\r\n--B  \r\nContent-Disposition: form-data; name="b"\r\n\r\n2\r\n--B--' > padding
  $ check padding
  param "a" "1"
  param "b" "2"

No part at all:

  $ printf %b '--B--\r\n' > no-part
  $ check no-part

Parameters around the boundary in the Content-Type, and the case of the
media type:

  $ check simple 'multipart/form-data; charset=utf-8; boundary=B'
  param "a" "1"
  file "f" "a.txt" - 6 bytes b1946ac92492d2347c6235b4d2611184
  $ check simple 'multipart/form-data;boundary=B;charset=utf-8'
  param "a" "1"
  file "f" "a.txt" - 6 bytes b1946ac92492d2347c6235b4d2611184
  $ check simple 'Multipart/Form-Data; boundary=B'
  param "a" "1"
  file "f" "a.txt" - 6 bytes b1946ac92492d2347c6235b4d2611184

Part headers in any case, and other part headers:

  $ printf %b '--B\r\ncontent-disposition: form-data; name="a"\r\n\r\n1\r\n--B\r\nCONTENT-DISPOSITION: FORM-DATA; NAME="f"; FILENAME="a.txt"\r\ncontent-type: text/plain\r\n\r\nx\r\n--B--\r\n' > case
  $ check case
  param "a" "1"
  file "f" "a.txt" text/plain 1 bytes 9dd4e461268c8034f5c8564e155c67a6
  $ printf %b '--B\r\nContent-Disposition: form-data; name="f"; filename="a.txt"\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Transfer-Encoding: binary\r\nX-Other: 1\r\n\r\nx\r\n--B\r\nContent-Type: text/plain\r\nContent-Disposition: form-data; name="a"\r\nContent-Transfer-Encoding: 8bit\r\n\r\n1\r\n--B--\r\n' > headers
  $ check headers
  param "a" "1"
  file "f" "a.txt" text/plain; charset=utf-8 1 bytes 9dd4e461268c8034f5c8564e155c67a6

A boundary as browsers make it, and one with characters special in regular
expressions:

  $ { part 'name="a"' 1 '----WebKitFormBoundary7MA4YWxkTrZu0gW'
  >   printf %s '------WebKitFormBoundary7MA4YWxkTrZu0gW--'; } > webkit
  $ check webkit 'multipart/form-data; boundary=----WebKitFormBoundary7MA4YWxkTrZu0gW'
  param "a" "1"
  $ { part 'name="a"' '\r\n--aXbbd\r\n' 'a.b+c?(d)'
  >   printf %s '--a.b+c?(d)--'; } > special
  $ check special 'multipart/form-data; boundary=a.b+c?(d)'
  param "a" "\r\n--aXbbd\r\n"

A quoted boundary, and a boundary parameter name not in lowercase, are not
supported yet, although RFC 2045 allows them: the body is rejected.

  $ check simple 'multipart/form-data; boundary="B"'
  status 400
  chunks.exe:
  error Bad request
  $ check simple 'multipart/form-data; Boundary=B'
  status 400
  chunks.exe:
  error Bad request

A line that starts with the delimiter (CRLF, "--" and the boundary) but is
not one, as "--BX" for the boundary "B", or a value that starts with "--B",
right after the CRLF of the empty line that ends the part headers. RFC 2046
forbids the delimiter inside a part, so such a body is malformed, but some
parsers read it as content. Clients choose a boundary absent from the
content:

  $ { part 'name="a"' '1\r\n--BX\r\n2'; printf %s '--B--'; } > not-delimiter
  $ check not-delimiter
  status 400
  chunks.exe:
  error Bad request
  $ { part 'name="a"' '--B is here'; printf %s '--B--'; } > not-delimiter2
  $ check not-delimiter2
  status 400
  chunks.exe:
  error Bad request

No file is left:

  $ ls uploads chunk-uploads
  chunk-uploads:
  
  uploads:

  $ stop_server $SERVER_PID
