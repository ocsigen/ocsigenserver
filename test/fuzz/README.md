# Property tests of the HTTP parsers

`fuzz.ml` checks properties of the parsers that read HTTP messages, with
[Crowbar](https://github.com/stedolan/crowbar): they never fail on any input,
they read back what the matching printers write, and they accept and refuse
what their documentation says.

- `Ocsigen_http.Header.Entity_tag`: `parse_list` reads the entity tags of a
  list, whatever the whitespace, the empty elements, the elements that are not
  entity tags and the lines; `make` accepts exactly the characters of an entity
  tag; the weak and strong comparisons follow RFC 9110.
- `Ocsigen_http.Header.Date.parse`: it reads the three date formats for any
  time from 1970 to 9999, with the 50-year rule for two-digit years, and
  reads a date iff its fields are valid.
- `Ocsigen_http.Http1`: `check_host` accepts valid hosts and absolute targets
  and refuses invalid characters and ports; `request_method` gives a token and
  skips empty lines; `is_tchar` and `is_field_value_char` follow RFC 9110.
- `Raw_http` (the client of the robustness tests): `responses` reads back
  responses framed by Content-Length or by the chunked coding (chunk sizes of
  one to three hexadecimal digits, extensions, trailers), after 1xx responses,
  with Connection: close; cut anywhere, it gives only the complete ones, and
  `framing` agrees with it, also on corrupted bytes.

`dune test` runs each property on 5000 random inputs. For more, each run
drawing new random inputs:

```
dune exec test/fuzz/fuzz.exe -- -r 1000000             # a million per property
while dune exec test/fuzz/fuzz.exe -- -r 1000000; do :; done   # until a failure
```

For a guided search with [AFL](https://aflplus.plus/), build in an opam switch
whose compiler instruments the code, such as
`opam switch create 5.4.0+afl ocaml-variants.5.4.0+options ocaml-option-afl`,
with the dependencies of the tests (`opam install . --deps-only --with-test`),
then give AFL a directory with a few seed files:

```
mkdir -p in && head -c 512 /dev/urandom > in/seed
afl-fuzz -i in -o out _build/default/test/fuzz/fuzz.exe @@
```

A failing input that AFL finds is in `out/default/crashes/`: run
`fuzz.exe FILE` to see which property fails, and on which value.
