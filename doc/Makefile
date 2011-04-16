include ../Makefile.config
include ../src/Makefile.filelist

OCAMLDOC := ${OCAMLFIND} ocamldoc

LIBS := -package lwt,tyxml,ssl \
        ${addprefix -I ../src/, baselib http server extensions }

doc: odoc

odoc:
	mkdir -p api-html
	$(OCAMLDOC) ${LIBS} -d api-html -intro indexdoc -html $(addprefix ../src/,$(DOC) $(PLUGINS_DOC))

install:
	${INSTALL} -d -m 755 $(TEMPROOT)$(DOCDIR)
	$(INSTALL) -m 644 api-html/* $(TEMPROOT)$(DOCDIR)

uninstall:
	-rm -Rf $(TEMPROOT)$(DOCDIR)

clean:
	-rm -f api-html/*
	-rm -f *~ \#* .\#*

