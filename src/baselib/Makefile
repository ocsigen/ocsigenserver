include ../../Makefile.config

LIBS     := -package lwt,netstring,cryptokit,findlib,xmlp4
OCAMLC   := $(OCAMLFIND) ocamlc${BYTEDBG}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG}
OCAMLDOC := $(OCAMLFIND) ocamldoc
OCAMLDEP := $(OCAMLFIND) ocamldep

all: byte opt

### Common files ###

FILES := ocsigen_lib_cli.ml        \
	 ocsigen_lib.ml            \
	 ocsigen_cache.ml          \
         ocsigen_config.ml         \
	 ocsigen_commandline.ml    \
	 ocsigen_messages.ml       \
	 dynlink_wrapper.ml        \
         ocsigen_loader.ml         \
	 ocsigen_stream.ml         \

PREDEP := ocsigen_config.ml dynlink_wrapper.ml

byte:: baselib.cma
opt:: baselib.cmxa

baselib.cma:
	${OCAMLC} -a -o $@ $^
baselib.cmxa: $(FILES:.ml=.cmx)
	${OCAMLOPT} -a -o $@ $^

baselib.cma: $(FILES:.ml=.cmo)

## Ocsigen_config ##

ifeq "$(NATDYNLINK)" "YES"
  NATIVECODE_RUNTIME_DETECT=Dynlink.is_native
else
  NATIVECODE_RUNTIME_DETECT=false
endif

VERSION := $(shell head -n 1 ../../VERSION)

ocsigen_config.ml: ocsigen_config.ml.in ../../Makefile.config ../../Makefile.options ../../VERSION
	cat ocsigen_config.ml.in \
	| sed s%0000000000000000%${VERSION}% \
	| sed s%_WARNING_%"Warning: this file has been generated from ocsigen_config.ml.in - DO NOT MODIFY MANUALLY!"% \
	| sed s%_LOGDIR_%$(LOGDIR)% \
	| sed s%_DATADIR_%$(DATADIR)%g \
	| sed s%_BINDIR_%$(BINDIR)%g \
	| sed s%_EXTLIBDIR_%$(EXTLIBDIR)%g \
	| sed s%_STATICPAGESDIR_%$(STATICPAGESDIR)% \
	| sed s%_UP_%$(UPLOADDIR)%g \
	| sed s%_OCSIGENUSER_%$(OCSIGENUSER)%g \
	| sed s%_OCSIGENGROUP_%$(OCSIGENGROUP)%g \
	| sed s%_PROJECTNAME_%$(PROJECTNAME)%g \
	| sed s%_COMMANDPIPE_%$(COMMANDPIPE)%g \
	| sed s%_CONFIGDIR_%$(CONFIGDIR)% \
	| sed s%_ISNATIVE_%$(NATIVECODE_RUNTIME_DETECT)%g \
	| sed "s%_DEPS_%$(INITPACKAGE)%g" \
	> ocsigen_config.ml

## Dynlink_wrapper ##

ifeq "$(NATDYNLINK)" "YES"
dynlink_wrapper.ml:
	ln -s dynlink_wrapper.natdynlink.ml $@
dynlink_wrapper.cmo dynlink_wrapper.cmx: dynlink_wrapper.natdynlink.ml
else
dynlink_wrapper.ml:
	ln -s dynlink_wrapper.nonatdynlink.ml $@
dynlink_wrapper.cmo dynlink_wrapper.cmx: dynlink_wrapper.nonatdynlink.ml
endif

### Command line ###

byte:: parsecommandline.cma donotparsecommandline.cma
opt:: parsecommandline.cmxa donotparsecommandline.cmxa
ifeq "$(NATDYNLINK)" "YES"
opt:: parsecommandline.cmxs donotparsecommandline.cmxs
endif

parsecommandline.cma: commandline/ocsigen_getcommandline.cmo
	$(OCAMLC) -a -o $@ $^

donotparsecommandline.cma: nocommandline/ocsigen_getcommandline.cmo
	$(OCAMLC) -a -o $@ $^

parsecommandline.cmxa: commandline/ocsigen_getcommandline.cmx
	$(OCAMLOPT) -a -o $@ $^

donotparsecommandline.cmxa: nocommandline/ocsigen_getcommandline.cmx
	$(OCAMLOPT) -a -o $@ $^

### Polytables ###

byte:: polytables.cma
opt:: polytables.cmxa
ifeq "$(NATDYNLINK)" "YES"
opt:: polytables.cmxs
endif

polytables.cma: polytables.cmo
	$(OCAMLC) -a -o $@ $<

polytables.cmxa: polytables.cmx
	$(OCAMLOPT) -a -o $@ $<

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
	-cd commandline; rm -f *.cm[iox]* *.annot *.o
	-cd nocommandline; rm -f *.cm[iox]* *.annot *.o

distclean: clean
	-rm -f *~ \#* .\#*

## Dependencies

depend: ${PREDEP}
	$(OCAMLDEP) ${LIBS} *.mli *.ml > .depend

FORCE:
-include .depend