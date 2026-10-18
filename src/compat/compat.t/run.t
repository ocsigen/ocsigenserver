The ocsigenserver-compat library alone provides the old module names of the
server, of the base library and of the HTTP layer.

  $ dune build ./test.exe 2>&1
  $ ./_build/default/test.exe
  a b
  host
  Ocsigen
