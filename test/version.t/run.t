--version prints the version of the server: "dev" in a plain dune build, the
contents of the VERSION file once configure has run.

  $ ocsigenserver --version | grep -E '^(dev|[0-9]+\.[0-9]+\.[0-9]+.*)$' | wc -l
  1
