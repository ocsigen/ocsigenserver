include ../../Makefile.config
PACKAGE  := \
	bytes \
	netstring \
	netstring-pcre \
	lwt.ssl \
	tyxml \
	cohttp

LIBS     := -I ../baselib ${addprefix -package ,${PACKAGE}}
OCAMLC   := $(OCAMLFIND) ocamlc ${BYTEDBG}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG}
OCAMLDOC := $(OCAMLFIND) ocamldoc
OCAMLDEP := $(OCAMLFIND) ocamldep

all: byte opt

### Common files ###

FILES := http_headers.ml         \
	 ocsigen_cookies.ml      \
	 ocsigen_charset_mime.ml

PREDEP :=

byte: http.cma
opt: http.cmxa

http.cma: $(FILES:.ml=.cmo)
	${OCAMLC} -a -o $@ $^
http.cmxa: $(FILES:.ml=.cmx)
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
