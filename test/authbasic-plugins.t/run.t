Authbasic tries its authentication methods in turn: a method registered
after <plain> is used for its own element, and its configuration errors
are reported as it gives them, both through get_basic_authentication_method
and from an <authbasic> element of the configuration.

  $ ../authbasic-plugins/test.exe 2>&1
  plain me/pw: true
  plain me/other: false
  always anyone/anything: true
  unknown: Unknown authentication method <ldap> in <authbasic>
  plain without password: <plain> takes the attributes login and password, in this order
  always with an attribute: <always> takes no attributes and no content
  <authbasic> with <always>: accepted
  <authbasic> with <always user="me">: <always> takes no attributes and no content
  <authbasic> with <ldap>: Unknown authentication method <ldap> in <authbasic>
