A configuration with an error leaves the settings of the tags of <server> as
they were. The same configuration without the error sets them, and a
configuration with only an error keeps them. Without the tags, they get back
the values they had before the first reading.

  $ ../reload-settings/test.exe
  tag <nosuchtag> unexpected inside <server>
  accepted
  tag <nosuchtag> unexpected inside <server>
  accepted
  <charset>: utf-8, after the error utf-8, then iso-8859-1, after the error iso-8859-1, without it utf-8
  <uploaddir>: none, after the error none, then upload, after the error upload, without it none
  <mimefile>: none, after the error none, then other.types, after the error other.types, without it none
  <maxconnected>: 10000, after the error 10000, then 7, after the error 7, without it 10000
  <maxretries>: 10, after the error 10, then 3, after the error 3, without it 10
  <clienttimeout>: 30, after the error 30, then 5, after the error 5, without it 30
  <servertimeout>: 30, after the error 30, then 6, after the error 6, without it 30
  <keepalivetimeout>: 75, after the error 75, then 20, after the error 20, without it 75
  <mindatarate>: 500, after the error 500, then 1000, after the error 1000, without it 500
  <maxconnectionsperaddress>: 0, after the error 0, then 16, after the error 16, without it 0
  <netbuffersize>: 8192, after the error 8192, then 4096, after the error 4096, without it 8192
  <filebuffersize>: 8192, after the error 8192, then 4096, after the error 4096, without it 8192
  <maxrequestbodysize>: 8000000, after the error 8000000, then 1000000, after the error 1000000, without it 8000000
  <maxrequestbodysizeinmemory>: 1048576, after the error 1048576, then 1000, after the error 1000, without it 1048576
  <maxuploadfilesize>: 2000000, after the error 2000000, then 1000, after the error 1000, without it 2000000
  <commandpipe>: none, after the error none, then local.cmd, after the error local.cmd, without it none
  <shutdowntimeout>: 10., after the error 10., then 1., after the error 1., without it 10.
  <debugmode>: false, after the error false, then true, after the error true, without it false
  <usedefaulthostname>: false, after the error false, then true, after the error true, without it false
  <disablepartialrequests>: false, after the error false, then true, after the error true, without it false
