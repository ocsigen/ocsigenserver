include ../Makefile.config
include ../Makefile.options

OCAMLDOC := ${OCAMLFIND} ocamldoc
ODOC_WIKI := odoc_wiki.cma

DOC := baselib/ocsigen_cache.mli          \
       baselib/ocsigen_lib_base.mli       \
       baselib/ocsigen_lib.mli            \
       baselib/ocsigen_config_static.mli  \
       baselib/ocsigen_stream.mli         \
       baselib/ocsigen_loader.mli         \
       baselib/polytables/polytables.mli  \
       \
       http/ocsigen_charset_mime.mli      \
       http/ocsigen_cookie_map.mli        \
       http/ocsigen_header.mli            \
       \
       server/ocsigen_config.mli          \
       server/ocsigen_request.mli         \
       server/ocsigen_response.mli        \
       server/ocsigen_messages.mli        \
       server/ocsigen_multipart.mli       \
       server/ocsigen_extensions.mli      \
       server/ocsigen_parseconfig.mli     \
       server/ocsigen_local_files.mli     \
       server/ocsigen_server.mli

PLUGINS_DOC := extensions/accesscontrol.mli extensions/authbasic.mli	\
	       extensions/cors.mli extensions/ocsipersist.mli		\
	       extensions/outputfilter.mli				\
	       extensions/redirectmod.mli extensions/staticmod.mli	\
	       extensions/deflatemod/deflatemod.mli

all: doc wikidoc

doc: api-html/index.html
api-html/index.html: indexdoc $(addprefix ../src/,$(DOC) $(PLUGINS_DOC))
	mkdir -p api-html
	$(OCAMLDOC) ${LIBS} -d api-html -intro indexdoc -html $(addprefix ../src/,$(DOC) $(PLUGINS_DOC))

wikidoc: api-wiki/index.wiki
api-wiki/index.wiki: indexdoc $(addprefix ../src/,$(DOC) $(PLUGINS_DOC))
	mkdir -p api-wiki
	$(OCAMLDOC) ${LIBS} -d api-wiki -intro indexdoc -colorize-code \
	   -i $(shell ocamlfind query wikidoc) -g ${ODOC_WIKI} \
	   $(addprefix ../src/,$(DOC) $(PLUGINS_DOC))

install:
	${INSTALL} -d -m 755 $(TEMPROOT)$(DOCDIR)
	$(INSTALL) -m 644 api-html/* $(TEMPROOT)$(DOCDIR)

uninstall:
	-rm -Rf $(TEMPROOT)$(DOCDIR)

clean:
	-rm -f api-html/* api-wiki/*
	-rm -f *~ \#* .\#*
