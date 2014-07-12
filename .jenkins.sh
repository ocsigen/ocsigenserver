opam pin add --no-action ocsigenserver .
opam install camlzip
opam install --deps-only ocsigenserver
opam install --verbose ocsigenserver
opam remove --verbose ocsigenserver
