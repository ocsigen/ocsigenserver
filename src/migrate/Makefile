include ../../Makefile.config

LIBS     := -package bytes,lwt,cohttp,netstring,re.emacs -I ../baselib -I ../http
OCAMLC   := $(OCAMLFIND) ocamlc${BYTEDBG}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG}
OCAMLDOC := $(OCAMLFIND) ocamldoc
OCAMLDEP := $(OCAMLFIND) ocamldep

all: byte opt

### Common files ###

FILES := of_cohttp.ml \
         to_cohttp.ml \

PREDEP := \

byte: migrate.cma
opt: migrate.cmxa

migrate.cma: $(FILES:.ml=.cmo)
	${OCAMLC} -a -o $@ $^
migrate.cmxa: $(FILES:.ml=.cmx)
	${OCAMLOPT} -a -o $@ $^

##########

%.ml: %.mll
	$(OCAMLLEX) $<
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
	-rm -f *.cm* *.o *.a *.annot
	-rm -f ${PREDEP}
distclean: clean
	-rm -f *~ \#* .\#*

## Dependencies

depend: ${PREDEP}
	$(OCAMLDEP) ${LIBS} *.mli *.ml > .depend

type: $(FILES:.ml=.gmli)

%.gmli: %.ml
	$(OCAMLC) ${LIBS} -i $< > $@


FORCE:
-include .depend
