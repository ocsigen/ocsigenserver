include ../../Makefile.config

PACKAGE  :=		\
	bytes		\
	lwt.unix	\
	ipaddr		\
	lwt.ssl		\
	pcre		\
	tyxml.parser    \
	cohttp.lwt

LIBS     := -I ../baselib -I ../http -I ../server \
            ${addprefix -package ,${PACKAGE}}
OCAMLC   := $(OCAMLFIND) ocamlc ${BYTEDBG}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG}
OCAMLDOC := $(OCAMLFIND) ocamldoc
OCAMLDEP := $(OCAMLFIND) ocamldep

all: byte opt

### Extensions ###

FILES := accesscontrol.ml authbasic.ml cors.ml extendconfiguration.ml \
	 outputfilter.ml redirectmod.ml revproxy.ml \
	 rewritemod.ml staticmod.ml userconf.ml

ifeq "$(CAMLZIP)" "YES"
FILES += deflatemod.ml
deflatemod.cmo deflatemod.cmx: LIBS+=-package ${CAMLZIPNAME}
endif

byte:: ${FILES:.ml=.cmo}
opt:: ${FILES:.ml=.cmx}
ifeq "$(NATDYNLINK)" "YES"
opt:: ${FILES:.ml=.cmxs}
endif

### PostgreSQL ###

ifeq "$(OCSIPERSISTPGSQL)" "YES"
byte::
	$(MAKE) -C ocsipersist-pgsql byte
opt::
	$(MAKE) -C ocsipersist-pgsql opt
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
	${MAKE} -C ocsipersist-pgsql clean
clean.local:
	-rm -f *.cm* *.o *.a *.annot
	-rm -f ${PREDEP}
distclean: clean.local
	-rm -f *~ \#* .\#*
	${MAKE} -C ocsipersist-dbm distclean
	${MAKE} -C ocsipersist-sqlite distclean
	${MAKE} -C ocsipersist-pgsql distclean
	-rm -f .depend

## Dependencies

depend: ${PREDEP}
	$(OCAMLDEP) ${LIBS} *.mli *.ml > .depend
	${MAKE} -C ocsipersist-dbm depend
	${MAKE} -C ocsipersist-sqlite depend
	${MAKE} -C ocsipersist-pgsql depend

FORCE:
-include .depend
