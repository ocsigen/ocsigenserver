include ../../../Makefile.config

PACKAGE  := ${LWT_PREEMPTIVE_PACKAGE} \
	    lwt.unix     \
	    tyxml.parser \
	    dbm          \

LIBS     := -I ../../baselib -I ../../http -I ../../server \
	    ${addprefix -package ,${PACKAGE}}
OCAMLC   := $(OCAMLFIND) ocamlc${BYTEDBG} ${THREAD}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG} ${THREAD}
OCAMLDOC := $(OCAMLFIND) ocamldoc
OCAMLDEP := $(OCAMLFIND) ocamldep

all: byte opt

###

byte:: ocsipersist-dbm.cma
opt:: ocsipersist-dbm.cmxa
ifeq "$(NATDYNLINK)" "YES"
opt:: ocsipersist-dbm.cmxs
endif

PREDEP := ocsipersist.mli

ocsipersist-dbm.cma: ocsipersist.cmo
	$(OCAMLC) -a -o $@ $^
	cp ocsipersist.cmi ..
	cp $@ ..

ocsipersist-dbm.cmxa: ocsipersist.cmx
	$(OCAMLOPT) -a -o $@ $^
	cp ocsipersist.cmi ..
	cp $@ ${patsubst %.cmxa,%.a,$@} ..

ocsipersist-dbm.cmxs: ocsipersist-dbm.cmxa
	$(OCAMLOPT) -shared -linkall -o $@ $^
	cp $@ ..

ocsipersist.mli:
	ln -s ../ocsipersist.mli .

###

byte:: ocsidbm
opt:: ocsidbm.opt

ocsidbm: ocsidbm.cmo
	$(OCAMLC) -linkpkg -o $@ ${LIBS} $^

ocsidbm.opt: ocsidbm.cmx
	$(OCAMLOPT) -linkpkg -o $@ ${LIBS} $^

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
	-rm -f ocsidbm ocsidbm.opt
distclean: clean
	-rm -f *~ \#* .\#*

## Dependencies

depend: ${PREDEP}
	$(OCAMLDEP) ${LIBS} *.mli *.ml > .depend

FORCE:
-include .depend
