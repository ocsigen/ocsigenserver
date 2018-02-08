include ../../../Makefile.config

PACKAGE  := lwt lwt.syntax pgocaml.syntax xml-light

LIBS     := -I ../../baselib -I ../../server ${addprefix -package ,${PACKAGE}}
OCAMLC   := $(OCAMLFIND) ocamlc${BYTEDBG} ${THREAD}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG} ${THREAD}
OCAMLDOC := $(OCAMLFIND) ocamldoc
OCAMLDEP := $(OCAMLFIND) ocamldep

all: byte opt

###

byte: ocsipersist-pgsql.cma
opt:: ocsipersist-pgsql.cmxa
ifeq "$(NATDYNLINK)" "YES"
opt:: ocsipersist-pgsql.cmxs
endif

PREDEP := ocsipersist.mli

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

##########

SYNTAX_FLAGS=-syntax camlp4o

%.cmi: %.mli
	$(OCAMLC) ${LIBS} -c $<
%.cmo: %.ml
	$(OCAMLC) ${LIBS} ${SYNTAX_FLAGS} -c $<
%.cmx: %.ml
	$(OCAMLOPT) ${LIBS} ${SYNTAX_FLAGS} -c $<
%.cmxs: %.cmxa
	$(OCAMLOPT) -shared -linkall -o $@ $<

## Clean up

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f ${PREDEP}
distclean: clean
	-rm -f *~ \#* .\#*
	-rm -f .depend

## Dependencies

depend: ${PREDEP}
	$(OCAMLDEP) ${LIBS} ${SYNTAX_FLAGS} *.mli *.ml > .depend

FORCE:
-include .depend
