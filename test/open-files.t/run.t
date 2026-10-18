Open_files raises the soft limit on open files, never lowers it, and keeps to
the hard limit.

  $ dune build ./limits.exe 2>&1
  $ (ulimit -Sn 256; ./_build/default/limits.exe)
  soft limit: 256
  raised to 300: 300
  not lowered to 100: 300
  raised further, up to the hard limit: true
  hard limit unchanged: true
  read again: true
