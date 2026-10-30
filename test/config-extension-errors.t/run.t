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
