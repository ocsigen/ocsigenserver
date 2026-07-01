Command-line error handling of the `ocsigenserver` binary. These checks start no
server, so they run on every platform (including Windows, where a background
server cannot be torn down cleanly inside a cram sandbox).

The unreproducible timestamp that prefixes the error messages is stripped, and
the exit code is checked separately so that it reflects ocsigenserver rather
than the filter.

Serve options require a directory to serve.

  $ ocsigenserver --port 8080 2>err; echo "exit: $?"
  exit: 2
  $ sed 's/^[A-Z][a-z][a-z] [ 0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]: //' err
  ocsigen:main: [ERROR] --port, --directory-listing and --command-pipe require a directory to serve

A non-existent directory is reported. The reported directory is made absolute,
so the (platform-dependent) leading path is normalised away.

  $ ocsigenserver --serve does-not-exist 2>err; echo "exit: $?"
  exit: 2
  $ sed -e 's/^[A-Z][a-z][a-z] [ 0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]: //' \
  >     -e 's#no such directory: .*[\\/]does-not-exist#no such directory: <DIR>/does-not-exist#' \
  >     err
  ocsigen:main: [ERROR] no such directory: <DIR>/does-not-exist

Serve mode and a configuration file cannot be combined.

  $ ocsigenserver --serve some-dir -c some.conf 2>err; echo "exit: $?"
  exit: 2
  $ sed 's/^[A-Z][a-z][a-z] [ 0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]: //' err
  ocsigen:main: [ERROR] -c/--config cannot be combined with serve mode
