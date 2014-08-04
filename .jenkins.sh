opam pin add --no-action ocsigenserver .
opam install camlzip
opam install --deps-only ocsigenserver
opam install --verbose ocsigenserver

./configure
make all
make -C src/sandbox/ all

do_build_doc () {
  make -C doc clean
  make -C doc doc
  make -C doc wikidoc
  cp -rf doc/api-wiki/*.wiki ${API_DIR}
  cp -rf doc/manual-wiki/*.wiki ${MANUAL_SRC_DIR}
}

do_remove () {
  opam remove --verbose ocsigenserver
}
