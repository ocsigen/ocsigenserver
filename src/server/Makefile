include ../../Makefile.config

all: byte opt

PACKAGE  := ${SERVER_PACKAGE} ${SERVER_SYNTAX} ## See ../../Makefile.options
LIBS     := -I ../baselib -I ../http ${addprefix -package ,${PACKAGE}} -I .
OCAMLC   := $(OCAMLFIND) ocamlc ${BYTEDBG} ${THREAD}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG} ${THREAD}
OCAMLDOC := $(OCAMLFIND) ocamldoc
OCAMLDEP := $(OCAMLFIND) ocamldep

all: byte opt

### Common files ###

FILES := ocsigen_socket.ml      \
         ocsigen_request_info.ml \
         ocsigen_command.ml     \
         ocsigen_range.ml       \
         ocsigen_extensions.ml  \
         ocsigen_parseconfig.ml \
         ocsigen_http_client.ml \
         ocsigen_local_files.ml \
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

${PROJECTNAME}: ${SERVERLIBS} ${SERVEROBJS}
	${OCAMLC} -o $@ -linkpkg -linkall ${THREAD} ${LIBS} $^
${PROJECTNAME}.opt: ${SERVERLIBS:.cma=.cmxa} ${SERVEROBJS:.cmo=.cmx}
	${OCAMLOPT} -o $@ -linkpkg -linkall ${THREAD} ${LIBS} $^

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
	-rm -f *.cm* *.o *.a *.annot
	-rm -f ${PREDEP}
	-rm -f ${PROJECTNAME} ${PROJECTNAME}.opt
	-rm -f servertop
distclean: clean
	-rm -f *~ \#* .\#*

## Dependencies

depend: ${PREDEP}
	$(OCAMLDEP) ${LIBS} *.mli *.ml > .depend

FORCE:
-include .depend
