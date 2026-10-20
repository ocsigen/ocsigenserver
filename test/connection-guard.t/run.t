Unit tests of Connection_guard (see ../connection-guard/test.ml): the clock is
driven by hand, one tick per second, and the reads and writes are promises
resolved by hand. With a timeout of 3 seconds, a connection expires after 4
ticks: since it may start just before a tick, it then has between 3 and 5
seconds, never less than its timeout.

  $ ../connection-guard/test.exe 2>&1
  Headers, timeout 3: expired after 4 ticks, in Headers
  Headers, one byte read each tick: expired after 4 ticks, in Headers
  Headers, timeout 0: not expired after 100 ticks
  Handshake, timeout 3: expired after 4 ticks, in Handshake
  Closing, timeout 3: expired after 4 ticks, in Closing
  
  Idle
  keep-alive timeout 5: closed after 6 ticks, the read failed with Timed_out, expired: false, on_expiry called: false
  a new read failed with Timed_out, a write returned 16
  the phase stays Closing; expired after 4 ticks, in Closing
  the same, never ended: expired after 10 ticks, in Closing
  keep-alive timeout 0: not expired after 100 ticks
  a byte after 4 ticks starts the headers: (now Headers) expired after 8 ticks, in Headers
  an empty read does not start the headers: expired after 10 ticks, in Closing
  
  Request
  no read pending (slow handler): not expired after 100 ticks
  a read stalled from tick 10: expired after 13 ticks, in Request
  the read failed with Timed_out
  one byte every 2 ticks, no minimum rate: not expired after 100 ticks
  one byte every 2 ticks, minimum rate 500 B/s: expired after 4 ticks, in Request
  the same, timeout 0: not expired after 100 ticks
  1000 bytes a tick, minimum rate 500 B/s: not expired after 100 ticks
  a fast body, then one byte every 2 ticks: expired after 24 ticks, in Request
  the same, the drip being a new request: expired after 14 ticks, in Request
  
  Writes, in any phase
  a write stalled while Idle (no keep-alive bound): expired after 4 ticks, in Idle
  the write failed with Timed_out
  a client reading 1 byte every 2 ticks, minimum rate 500 B/s: expired after 4 ticks, in Request
  a write stalled, timeout 0, minimum rate 500 B/s: not expired after 100 ticks
  
  Closing after a response
  the pending read failed with Timed_out, expired: false, on_expiry called: false, phase: Closing
  a new read failed with Timed_out, a write returned 16
  never ended: expired after 4 ticks, in Closing
  
  After the expiry
  expired: true
  a new read failed with Timed_out
  the phase stays Headers
  a read completing after the expiry failed with Timed_out
  a handshake still pending at the expiry failed with Timed_out
  an exception from on_expiry: the read failed with Timed_out
  
  Making room, capacity 4
  idle, busy and silent connections, then a fourth:
  the idle one: Closing
  its read failed with Timed_out
  the busy one: Request
  the silent one: Headers
  a connection in its TLS handshake comes:
  the silent one: Closing
  its read failed with Timed_out
  the fourth: Headers
  another comes:
  the handshake: Closing
  the handshake failed with Timed_out
  another comes, all the others being busy:
  the busy one: Request
  the fourth: Headers
  the sixth: Headers
  the server is full; the fourth becomes idle, and a tick passes:
  the fourth: Closing
  the seventh: Headers
  
  Connections per address
  acquire 192.0.2.1: true
  acquire 192.0.2.1: true
  acquire 192.0.2.1: false
  acquire 192.0.2.2: true
  acquire 192.0.2.1: true
  acquire ::ffff:192.0.2.2: true
  acquire ::ffff:192.0.2.2: false
  acquire 2001:db8:1:2::1: true
  acquire 2001:db8:1:2:ffff::7: true
  acquire 2001:db8:1:2:abcd::1: false
  acquire 2001:db8:1:3::1: true
  count 2001:db8:1:2::42: 2
  count 192.0.2.1 after its releases: 0
  no limit: true
