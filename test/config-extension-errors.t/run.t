An element or an attribute that an extension rejects in the configuration
file stops the server at start-up, with a message that says which one.

  $ mkdir -p log
  $ cat > attr.conf <<CONF
  > <ocsigen><server>
  > <port>0</port>
  > <logdir>log</logdir>
  > <extension findlib-package="ocsigenserver.ext.cors"/>
  > <host hostfilter="*"><cors foo="1"/></host>
  > </server></ocsigen>
  > CONF
  $ ocsigenserver -c attr.conf >out.log 2>&1; echo "exit $?"
  exit 50
  $ grep -o 'Fatal - .*' out.log
  Fatal - Error in configuration file: Unexpected attribute foo in tag cors

  $ cat > missing.conf <<CONF
  > <ocsigen><server>
  > <port>0</port>
  > <logdir>log</logdir>
  > <extension findlib-package="ocsigenserver.ext.deflatemod"/>
  > <host hostfilter="*"><deflate/></host>
  > </server></ocsigen>
  > CONF
  $ ocsigenserver -c missing.conf >out.log 2>&1; echo "exit $?"
  exit 50
  $ grep -o 'Fatal - .*' out.log
  Fatal - Error in configuration file: Obligatory attribute compress not in tag deflate

A wrong value of an attribute of <cors> is reported with what is expected:

  $ cat > cors.conf <<CONF
  > <ocsigen><server>
  > <port>0</port>
  > <logdir>log</logdir>
  > <extension findlib-package="ocsigenserver.ext.cors"/>
  > <host hostfilter="*"><cors credentials="yes"/></host>
  > </server></ocsigen>
  > CONF
  $ ocsigenserver -c cors.conf >out.log 2>&1; echo "exit $?"
  exit 50
  $ grep -o 'Fatal - .*' out.log
  Fatal - Error in configuration file: The credentials attribute of <cors> must be true or false, not "yes"
  $ sed 's/credentials="yes"/max_age="-1"/' cors.conf > cors2.conf
  $ ocsigenserver -c cors2.conf >out.log 2>&1; echo "exit $?"
  exit 50
  $ grep -o 'Fatal - .*' out.log
  Fatal - Error in configuration file: The max_age attribute of <cors> must be a non-negative number of seconds, not "-1"
  $ sed 's/credentials="yes"/max_age="one day"/' cors.conf > cors3.conf
  $ ocsigenserver -c cors3.conf >out.log 2>&1; echo "exit $?"
  exit 50
  $ grep -o 'Fatal - .*' out.log
  Fatal - Error in configuration file: The max_age attribute of <cors> must be a non-negative number of seconds, not "one day"
