Authbasic tries its authentication methods in turn: a method registered
after <plain> is used for its own element.

  $ dune exec ./test.exe 2>&1
  plain me/pw: true
  plain me/other: false
  always anyone/anything: true
  unknown: Unknown authentication method <ldap> in <authbasic>
  bad plain: <plain> takes the attributes login and password, in this order
