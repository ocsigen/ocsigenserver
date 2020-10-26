include ../../Makefile.config

PACKAGE  := ${SERVER_PACKAGE} ## See ../../Makefile.options
LIBS     := -I ../baselib -I ../http ${addprefix -package ,${PACKAGE}}

.PHONY: default
default: build

.PHONY: build
build:
	dune build

## Toplevel

SERVERLIBS := ../baselib/baselib.cma    \
	      ../baselib/polytables.cma \
	      ../http/http.cma          \
	      ${PROJECTNAME}.cma         \

top: servertop
	OCAMLPATH=${SRC}/src/files/:${OCAMLPATH} ${RLWRAP} ./servertop

servertop: ${SERVERLIBS}
	OCAMLPATH=${SRC}/src/files/:${OCAMLPATH} ${OCAMLFIND} ocamlmktop \
	   -o $@ -linkall -linkpkg ${THREAD} ${LIBS} $^

## Clean up

clean:
	dune clean

distclean: clean
	-rm -f *~ \#* .\#*
	-rm -f servertop
