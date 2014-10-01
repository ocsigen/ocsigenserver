cat opam
opam pin add --no-action ocsigenserver .
opam install camlzip
cat opam
opam install --deps-only ocsigenserver
opam install --verbose ocsigenserver
cat opam

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
