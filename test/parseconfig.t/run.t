Size-valued configuration tags: the units accepted by parse_size_tag, and the
wiring of each tag to its Config value. Also, the warning for a tag that has no
effect.

  $ dune exec ./test.exe 2>&1
  parse_size_tag:
  "8192"       -> 8192
  "8192B"      -> 8192
  "8192o"      -> 8192
  "10kB"       -> 10000
  "10ko"       -> 10000
  "10kiB"      -> 10240
  "10kio"      -> 10240
  "2MB"        -> 2000000
  "2MiB"       -> 2097152
  "1GB"        -> 1000000000
  "1TB"        -> 1000000000000
  "1k"         -> 1024
  "1M"         -> 1048576
  "infinity"   -> None
  ""           -> None
  "not-a-size" -> Config_file_error
  
  default in-memory request body size -> 1048576
  
  tags:
  <maxrequestbodysize>2MB -> 2000000
  <maxrequestbodysize>infinity -> None
  <maxuploadfilesize>3MB -> 3000000
  <maxrequestbodysizeinmemory>1MB -> 1000000
  <maxrequestbodysizeinmemory>infinity -> max_int
  
  tags without effect:
  warning: Config file: <servertimeout> has no effect and is ignored.
