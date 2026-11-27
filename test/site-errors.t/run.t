The errors in the attributes and the content of <site> stop the server at
start-up, with a message that says what is wrong.

  $ mkdir -p log
  $ check () {
  >   printf '<ocsigen><server><port>0</port><logdir>log</logdir>' > conf.xml
  >   printf '<host hostfilter="*">%s</host></server></ocsigen>' "$1" >> conf.xml
  >   ocsigenserver -c conf.xml >out.log 2>&1; echo "exit $?"
  >   grep -o 'Fatal - .*' out.log
  > }

  $ check '<site charset="utf-8"></site>'
  exit 50
  Fatal - Error in configuration file: Missing path attribute in <site>
  $ check '<site path="a" dir="b"></site>'
  exit 50
  Fatal - Error in configuration file: Duplicate attribute path (or dir) in <site>
  $ check '<site path="a" color="blue"></site>'
  exit 50
  Fatal - Error in configuration file: Wrong attribute for <site>: color
  $ check '<site path="a">some text</site>'
  exit 50
  Fatal - Error in configuration file: Unexpected text inside <host> or <site>

A tag that no extension knows is reported with the path of the site, in the
URLs, that it configures:

  $ check '<nosuchtag/>'
  exit 50
  Fatal - Error in configuration file: Unexpected tag <nosuchtag> in the configuration of /
  $ check '<site path="a"><site path="b"><nosuchtag/></site></site>'
  exit 50
  Fatal - Error in configuration file: Unexpected tag <nosuchtag> in the configuration of /a/b
