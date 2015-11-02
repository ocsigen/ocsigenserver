include ../../Makefile.config

PACKAGE  :=		\
	bytes		\
	lwt.unix	\
	ipaddr		\
	lwt.ssl		\
	lwt.react	\
	netstring	\
	netstring-pcre	\
	tyxml.parser	\
	${SERVER_SYNTAX} ## See ../../Makefile.options

LIBS     := -I ../baselib -I ../http -I ../server ${addprefix -package ,${PACKAGE}}
OCAMLC   := $(OCAMLFIND) ocamlc ${BYTEDBG}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG}
OCAMLDOC := $(OCAMLFIND) ocamldoc
OCAMLDEP := $(OCAMLFIND) ocamldep

all: byte opt

### Extensions ###

FILES := staticmod.ml           \
	 cgimod.ml              \
	 redirectmod.ml         \
	 revproxy.ml            \
	 extensiontemplate.ml   \
         accesscontrol.ml       \
         userconf.ml            \
	 outputfilter.ml        \
	 authbasic.ml           \
	 rewritemod.ml          \
	 extendconfiguration.ml \
         ocsigen_comet.ml       \
         cors.ml                \

ifeq "$(CAMLZIP)" "YES"
FILES += deflatemod.ml
deflatemod.cmo deflatemod.cmx: LIBS+=-package ${CAMLZIPNAME}
endif

byte:: ${FILES:.ml=.cmo}
opt:: ${FILES:.ml=.cmx}
ifeq "$(NATDYNLINK)" "YES"
opt:: ${FILES:.ml=.cmxs}
endif

### SQLite ###

ifeq "$(OCSIPERSISTSQLITE)" "YES"
byte::
	$(MAKE) -C ocsipersist-sqlite byte
opt::
	$(MAKE) -C ocsipersist-sqlite opt
endif

### DBM ####

ifeq "$(OCSIPERSISTDBM)" "YES"
byte::
	$(MAKE) -C ocsipersist-dbm byte
opt::
	$(MAKE) -C ocsipersist-dbm opt
endif

##########

%.cmi: %.mli
	$(OCAMLC) ${LIBS} -c $<
%.cmo: %.ml
	$(OCAMLC) ${LIBS} -c $<
%.cmx: %.ml
	$(OCAMLOPT) ${LIBS} -c $<
%.cmxs: %.cmx
	$(OCAMLOPT) -shared -linkall -o $@ $<

## Clean up

clean: clean.local
	${MAKE} -C ocsipersist-dbm clean
	${MAKE} -C ocsipersist-sqlite clean
clean.local:
	-rm -f *.cm* *.o *.a *.annot
	-rm -f ${PREDEP}
distclean: clean.local
	-rm -f *~ \#* .\#*
	${MAKE} -C ocsipersist-dbm distclean
	${MAKE} -C ocsipersist-sqlite distclean

## Dependencies

depend: ${PREDEP}
	$(OCAMLDEP) ${LIBS} *.mli *.ml > .depend
	${MAKE} -C ocsipersist-dbm depend
	${MAKE} -C ocsipersist-sqlite depend

FORCE:
-include .depend
