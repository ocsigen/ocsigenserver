include ../../Makefile.config

all: byte

PACKAGE  := ${SERVER_PACKAGE} ${SERVER_SYNTAX} lwt.react ## See ../../Makefile.options
LIBS     := -I ../baselib -I ../http ${addprefix -package ,${PACKAGE}} \
	    -I ../server -I ../extensions/
OCAMLC   := $(OCAMLFIND) ocamlc${BYTEDBG} ${THREAD}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG} ${THREAD}
OCAMLDOC := $(OCAMLFIND) ocamldoc
OCAMLDEP := $(OCAMLFIND) ocamldep

all: byte

byte:: ${PROJECTNAME}

ifdef DONOTPARSECOMMANDLINE
PARSECOMMANDLINE := ../baselib/donotparsecommandline.cma
else
PARSECOMMANDLINE := ../baselib/parsecommandline.cma
endif

SERVERLIBS := ${PARSECOMMANDLINE}       \
              ../baselib/baselib.cma    \
	      ../baselib/polytables.cma \
	      ../http/http.cma          \
	      ../server/${PROJECTNAME}.cma         \

SERVEROBJS := ../extensions/accesscontrol.cmo \
	      ../extensions/authbasic.cmo \
	      ../extensions/cgimod.cmo \
	      ../extensions/cors.cmo \
	      ../extensions/extendconfiguration.cmo \
	      ../extensions/extensiontemplate.cmo \
	      ../extensions/ocsigen_comet.cmo \
	      ../extensions/outputfilter.cmo \
	      ../extensions/redirectmod.cmo \
	      ../extensions/revproxy.cmo \
	      ../extensions/rewritemod.cmo \
	      ../extensions/staticmod.cmo \
	      ../extensions/userconf.cmo \

ifeq "$(CAMLZIP)" "YES"
SERVEROBJS += ../extensions/deflatemod.cmo
LIBS += -package ${CAMLZIPNAME}
endif

SERVEROBJS += server_main.cmo


${PROJECTNAME}: ${SERVERLIBS} ${SERVEROBJS}
	${OCAMLC} -o $@ -linkpkg -linkall ${THREAD} ${LIBS} $^

### Toplevel ####

top: servertop
	OCAMLPATH=${SRC}/src/files/:${OCAMLPATH} ${RLWRAP} ./servertop

servertop: ${SERVERLIBS}
	OCAMLPATH=${SRC}/src/files/:${OCAMLPATH} ${OCAMLFIND} ocamlmktop \
	   -o $@ -linkall -linkpkg ${THREAD} ${LIBS} $^

##########

%.cmi: %.mli
	$(OCAMLC) ${LIBS} -c $<
%.cmo: %.ml
	$(OCAMLC) ${LIBS} -c $<
%.cmx: %.ml
	$(OCAMLOPT) ${LIBS} -c $<
%.cmxs: %.cmxa
	$(OCAMLOPT) -shared -linkall -o $@ $<
%.o: %.c
	$(OCAMLC) -c $<

## Clean up

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f ${PREDEP}
	-rm -r ${PROJECTNAME}
	-rm -f servertop
distclean: clean
	-rm -f *~ \#* .\#*

## Dependencies

depend: ${PREDEP}
	$(OCAMLDEP) ${LIBS} *.mli *.ml > .depend

FORCE:
-include .depend
