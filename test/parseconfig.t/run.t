Size-valued configuration tags: the units accepted by parse_size_tag, and the
wiring of each tag to its Config value. Then the log levels set by <loglevel>
and by the logs: command of the command pipe.

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
  
  loglevel:
  default -> warning
  <loglevel source="test:app" level="debug"/> -> debug
  <loglevel level="info" source="test:app"/> -> info
  <loglevel source="test:app" level="loud"/> -> Config_file_error: <loglevel level="loud">: expected debug, info, notice, warning, error or fatal
  <loglevel level="debug"/> -> Config_file_error: <loglevel> needs a source attribute
  <loglevel source="test:none" level="debug"/> -> info
  logs:test:app warning -> warning
  logs:test:app -> quiet
  logs:test:app loud -> quiet
  sources named test:app -> 1
