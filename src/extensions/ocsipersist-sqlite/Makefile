.PHONY: default
default: build

.PHONY: build
build:
	dune build

###

ocsipersist-sqlite.cma: ocsipersist.cmo
	$(OCAMLC) -a -o $@ $^
	cp ocsipersist.cmi ..
	cp $@ ..

ocsipersist-sqlite.cmxa: ocsipersist.cmx
	$(OCAMLOPT) -a -o $@ $^
	cp ocsipersist.cmi ..
	cp $@ ${patsubst %.cmxa,%.a,$@} ..

ocsipersist-sqlite.cmxs: ocsipersist-sqlite.cmxa
	$(OCAMLOPT) -shared -linkall -o $@ $^
	cp $@ ..

ocsipersist.mli:
	ln -s -f ../ocsipersist.mli .

## Clean up

clean:
	dune clean

distclean: clean
	-rm -f *~ \#* .\#*
