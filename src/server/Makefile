include ../../Makefile.options
include ../../Makefile.config

.PHONY: default
default: build

.PHONY: build
build:
	dune build

## Toplevel

SERVERLIBS := ${BLD}/baselib/ocsigen_lib_base.cma      \
	      ${BLD}/baselib/baselib.cma               \
	      ${BLD}/baselib/polytables/polytables.cma \
	      ${BLD}/http/http.cma                     \
	      ${BLD}/http/ocsigen_cookie_map.cma       \
	      ${BLD}/server/ocsigenserver.cma

top: servertop
	OCAMLPATH=${SRC}/src/files/:${OCAMLPATH} ${RLWRAP} ./servertop

servertop: build
	OCAMLPATH=${SRC}/src/files/:${OCAMLPATH} ${OCAMLFIND} ocamlmktop \
	   -o $@ -linkall -linkpkg ${THREAD} ${LIBS} ${SERVERLIBS}

## Clean up

clean:
	dune clean

distclean: clean
	-rm -f *~ \#* .\#*
	-rm -f servertop
