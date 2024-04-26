include ../../Makefile.config

.PHONY: default
default: build

.PHONY: build
build: confs
	dune build

.PHONY: confs
confs: ocsigen_config_static.ml

## Ocsigen_config ##

VERSION := $(shell head -n 1 ../../VERSION)

ocsigen_config_static.ml: ../files/ocsigenserver.conf/options.ml
	dune exec -- ../files/ocsigenserver.conf/gen.exe static.ml > "$@"

## Clean up

clean:
	-rm -f ocsigen_config_static.ml

distclean: clean
	-rm -f *~ \#* .\#*
