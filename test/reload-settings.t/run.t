A configuration with an error leaves the settings of the tags of <server> as
they were. The same configuration without the error sets them.

  $ ../reload-settings/test.exe
  tag <nosuchtag> unexpected inside <server>
  accepted
  <charset>: none, after the error none, then iso-8859-1
  <uploaddir>: none, after the error none, then upload
  <mimefile>: none, after the error none, then other.types
  <maxconnected>: 10000, after the error 10000, then 7
  <maxretries>: 10, after the error 10, then 3
  <clienttimeout>: 30, after the error 30, then 5
  <servertimeout>: 30, after the error 30, then 6
  <keepalivetimeout>: 75, after the error 75, then 20
  <mindatarate>: 500, after the error 500, then 1000
  <maxconnectionsperaddress>: 0, after the error 0, then 16
  <netbuffersize>: 8192, after the error 8192, then 4096
  <filebuffersize>: 8192, after the error 8192, then 4096
  <maxrequestbodysize>: 8000000, after the error 8000000, then 1000000
  <maxrequestbodysizeinmemory>: 1048576, after the error 1048576, then 1000
  <maxuploadfilesize>: 2000000, after the error 2000000, then 1000
  <commandpipe>: none, after the error none, then local.cmd
  <shutdowntimeout>: 10., after the error 10., then 1.
  <debugmode>: false, after the error false, then true
  <usedefaulthostname>: false, after the error false, then true
  <disablepartialrequests>: false, after the error false, then true
