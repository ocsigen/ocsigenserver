include ../Makefile.config
include ../src/Makefile.filelist

OCAMLDOC := ${OCAMLFIND} ocamldoc
ODOC_WIKI := odoc_wiki.cma

LIBS := -package lwt,tyxml,ssl,netstring,netstring-pcre,ipaddr \
        ${addprefix -I ../src/, baselib http server extensions }

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

