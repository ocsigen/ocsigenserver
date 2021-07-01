include ../../Makefile.config

.PHONY: default
default: build

.PHONY: build
build::
	dune build

## Clean up

distclean: clean
	-rm -f *~ \#* .\#*
	${MAKE} -C deflatemod distclean
