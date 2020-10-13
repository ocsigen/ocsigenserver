.PHONY: default
default: build

.PHONY: build
build:
	dune build

###

ocsipersist-pgsql.cma: ocsipersist.cmo
	$(OCAMLC) -a -o $@ $^
	cp ocsipersist.cmi ..
	cp $@ ..

ocsipersist-pgsql.cmxa: ocsipersist.cmx
	$(OCAMLOPT) -a -o $@ $^
	cp ocsipersist.cmi ..
	cp $@ ${patsubst %.cmxa,%.a,$@} ..

ocsipersist-pgsql.cmxs: ocsipersist-pgsql.cmxa
	$(OCAMLOPT) -shared -linkall -o $@ $^
	cp $@ ..

ocsipersist.mli:
	ln -s -f ../ocsipersist.mli .

## Clean up

clean:
	dune clean

distclean: clean
	-rm -f *~ \#* .\#*
