include ../../Makefile.config

all: byte opt

PACKAGE  := ${SERVERPACKAGE} ## See ../../Makefile.options
LIBS     := -I ../baselib -I ../http ${addprefix -package ,${PACKAGE}}
OCAMLC   := $(OCAMLFIND) ocamlc${BYTEDBG} -thread
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG} -thread
OCAMLDOC := $(OCAMLFIND) ocamldoc
OCAMLDEP := $(OCAMLFIND) ocamldep

all: byte opt

### Common files ###

FILES := ocsigen_extensions.ml  \
         ocsigen_range.ml       \
         ocsigen_parseconfig.ml \
         ocsigen_http_client.ml \
         ocsigen_local_files.ml  \
         ocsigen_server.ml      \

byte:: ${PROJECTNAME}.cma
opt:: ${PROJECTNAME}.cmxa

${PROJECTNAME}.cma: $(FILES:.ml=.cmo)
	${OCAMLC} -a -o $@ $^
${PROJECTNAME}.cmxa: $(FILES:.ml=.cmx)
	${OCAMLOPT} -a -o $@ $^

### Server ###

byte:: ${PROJECTNAME}
opt:: ${PROJECTNAME}.opt

ifdef DONOTPARSECOMMANDLINE
PARSECOMMANDLINE := ../baselib/donotparsecommandline.cma
else
PARSECOMMANDLINE := ../baselib/parsecommandline.cma
endif

SERVERLIBS := ${PARSECOMMANDLINE}       \
              ../baselib/baselib.cma    \
	      ../baselib/polytables.cma \
	      ../http/http.cma          \
	      ${PROJECTNAME}.cma         \

SERVEROBJS := server_main.cmo
STUBSOBJS  := stubs.o

${PROJECTNAME}: ${SERVERLIBS} ${SERVEROBJS} ${STUBSOBJS}
	${OCAMLC} -o $@ -linkpkg -linkall -thread ${LIBS} $^
${PROJECTNAME}.opt: ${SERVERLIBS:.cma=.cmxa} ${SERVEROBJS:.cmo=.cmx} ${STUBSOBJS}
	${OCAMLOPT} -o $@ -linkpkg -linkall -thread ${LIBS} $^

### Toplevel ####

top: servertop
	OCAMLPATH=${SRC}/src/files/:${OCAMLPATH} ${RLWRAP} ./servertop

servertop: ${SERVERLIBS} ${STUBSOBJS}
	OCAMLPATH=${SRC}/src/files/:${OCAMLPATH} ${OCAMLFIND} ocamlmktop \
	   -o $@ -linkall -linkpkg -thread ${LIBS} $^

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
	-rm -r ${PROJECTNAME} ${PROJECTNAME}.opt
	-rm -f servertop
distclean: clean
	-rm -f *~ \#* .\#*

## Dependencies

depend: ${PREDEP}
	$(OCAMLDEP) ${LIBS} *.mli *.ml > .depend

FORCE:
-include .depend
