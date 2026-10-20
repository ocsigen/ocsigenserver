Size-valued configuration tags: the units accepted by parse_size_tag, and the
wiring of each tag to its Config value. Then the defaults and the wiring of the
connection tags and of <commandpipe> and <logdir>, and the log levels set by
<loglevel> and by the logs: command of the command pipe. Then the warnings for
the tags that have no effect. Last, the log levels set by the -vv option.

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
  
  defaults -> keepalivetimeout 75, mindatarate 500, maxconnectionsperaddress 0
  <keepalivetimeout>20 -> 20
  <mindatarate>1000 -> 1000
  <maxconnectionsperaddress>16 -> 16
  
  default command pipe -> none
  <commandpipe>local.cmd -> local.cmd
  
  default log directory -> none
  <logdir>log -> log
  no syslog -> log
  <logdir> -> none
  
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
  
  tags without effect:
  warning: Config file: <servertimeout> has no effect and is ignored.
  warning: Config file: <datadir> has no effect and is ignored. Ocsipersist (DBM or SQLite) takes its location from its own <store> or <database> element.
  
  -vv:
  existing Ocsigen source -> info
  existing other source -> warning
  source created afterwards -> info
