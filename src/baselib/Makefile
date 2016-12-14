include ../../Makefile.config

PACKAGE  := \
	bytes \
	lwt.unix \
	lwt_log \
	cryptokit \
	findlib \
	pcre \
	hmap \
	ipaddr

LIBS     := ${addprefix -package ,${PACKAGE}}
# -no-keep-locs is needed since OCaml 4.06. If we don't compile with
# -no-keep-locs, the .cmi's of the different ocsigen_*commandline will
# be incompatible with each other leading to a compilation failure
# later.
OCAMLC   := $(OCAMLFIND) ocamlc -no-keep-locs ${BYTEDBG} ${THREAD}
OCAMLOPT := $(OCAMLFIND) ocamlopt -no-keep-locs ${OPTDBG} ${THREAD}
OCAMLDOC := $(OCAMLFIND) ocamldoc
OCAMLDEP := $(OCAMLFIND) ocamldep

all: byte opt

### Common files ###

FILES := ocsigen_lib_base.ml       \
	 ocsigen_lib.ml            \
	 ocsigen_cache.ml          \
         ocsigen_config_static.ml  \
	 dynlink_wrapper.ml        \
         ocsigen_loader.ml         \
	 ocsigen_stream.ml         \

INTF_NOP4 := ocsigen_lib_base.mli ocsigen_lib.mli

PREDEP := ocsigen_config_static.ml dynlink_wrapper.ml

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

ocsigen_config_static.ml: ocsigen_config_static.ml.in ../../Makefile.config ../../Makefile.options ../../VERSION
	cat ocsigen_config_static.ml.in \
	| sed s%_VERSION_%${VERSION}% \
	| sed s%_WARNING_%"Warning: this file has been generated from ocsigen_config_static.ml.in - DO NOT MODIFY MANUALLY!"% \
	| sed s%_LOGDIR_%$(LOGDIR)% \
	| sed s%_DATADIR_%$(DATADIR)%g \
	| sed s%_BINDIR_%$(BINDIR)%g \
	| sed s%_EXTDIR_%$(LIBDIR)/${PROJECTNAME}/extensions%g \
	| sed s%_STATICPAGESDIR_%$(STATICPAGESDIR)% \
	| sed s%_UP_%$(UPLOADDIR)%g \
	| sed s%_OCSIGENUSER_%$(OCSIGENUSER)%g \
	| sed s%_OCSIGENGROUP_%"$(OCSIGENGROUP)"%g \
	| sed s%_PROJECTNAME_%$(PROJECTNAME)%g \
	| sed s%_COMMANDPIPE_%$(COMMANDPIPE)%g \
	| sed s%_CONFIGDIR_%$(CONFIGDIR)% \
	| sed s%_ISNATIVE_%$(NATIVECODE_RUNTIME_DETECT)%g \
	| sed "s%_DEPS_%$(INITPACKAGE)%g" \
	> ocsigen_config_static.ml

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
${INTF_NOP4:.mli=.cmi}: \
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
	$(OCAMLDEP) ${LIBS} $(filter-out ${INTF_NOP4},$(wildcard *.mli)) *.ml > .depend
	$(OCAMLDEP) ${LIBS} ${INTF_NOP4} >> .depend

FORCE:
-include .depend
