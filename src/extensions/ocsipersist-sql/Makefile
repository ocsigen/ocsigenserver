include ../../../Makefile.config

PACKAGE  := lwt.preemptive  \
	    tyxml.parser \
	    lwt.ppx \
	    macaque.syntax \
	    pgocaml.syntax \

LIBS     := -I ../../baselib -I ../../http -I ../../server \
	    ${addprefix -package ,${PACKAGE}}
OCAMLC   := $(OCAMLFIND) ocamlc${BYTEDBG} ${THREAD}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG} ${THREAD}
OCAMLDOC := $(OCAMLFIND) ocamldoc
OCAMLDEP := $(OCAMLFIND) ocamldep

all: byte opt

###

byte: ocsipersist-sql.cma
opt:: ocsipersist-sql.cmxa
ifeq "$(NATDYNLINK)" "YES"
opt:: ocsipersist-sql.cmxs
endif

PREDEP := ocsipersist.mli

ocsipersist-sql.cma: ocsipersist.cmo
	$(OCAMLC) -a -o $@ $^
	cp ocsipersist.cmi ..
	cp $@ ..

ocsipersist-sql.cmxa: ocsipersist.cmx
	$(OCAMLOPT) -a -o $@ $^
	cp ocsipersist.cmi ..
	cp $@ ${patsubst %.cmxa,%.a,$@} ..

ocsipersist-sql.cmxs: ocsipersist-sql.cmxa
	$(OCAMLOPT) -shared -linkall -o $@ $^
	cp $@ ..

ocsipersist.mli:
	ln -s ../ocsipersist.mli .

##########

%.cmi: %.mli
	$(OCAMLC) ${LIBS} -c $<
%.cmo: %.ml
	$(OCAMLC) ${LIBS} -c $<
%.cmx: %.ml
	$(OCAMLOPT) ${LIBS} -c $<
%.cmxs: %.cmxa
	$(OCAMLOPT) -shared -linkall -o $@ $<

## Clean up

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f ${PREDEP}
distclean: clean
	-rm -f *~ \#* .\#*

## Dependencies

depend: ${PREDEP}
	$(OCAMLDEP) ${LIBS} *.mli *.ml > .depend

FORCE:
-include .depend
