include ../Makefile.config
include ../src/Makefile.filelist

BLD=../_build/default/src

INCS=   -I ${BLD}/server/.ocsigenserver.objs/byte \
	-I ${BLD}/http/.http.objs/byte \
	-I ${BLD}/http/.ocsigen_cookie_map.objs/byte \
	-I ${BLD}/baselib/.baselib.objs/byte \
	-I ${BLD}/baselib/.ocsigen_lib_base.objs/byte \
	-I ${BLD}/baselib/polytables/.polytables.objs/byte \
	-I ${BLD}/extensions/.accesscontrol.objs/byte \
	-I ${BLD}/extensions/.authbasic.objs/byte \
	-I ${BLD}/extensions/.cors.objs/byte \
	-I ${BLD}/extensions/.extendconfiguration.objs/byte \
	-I ${BLD}/extensions/.ocsigenpersist.objs/byte \
	-I ${BLD}/extensions/.outputfilter.objs/byte \
	-I ${BLD}/extensions/.redirectmod.objs/byte \
	-I ${BLD}/extensions/.revproxy.objs/byte \
	-I ${BLD}/extensions/.rewritemod.objs/byte \
	-I ${BLD}/extensions/.staticmod.objs/byte \
	-I ${BLD}/extensions/.userconf.objs/byte \
	-I ${BLD}/extensions/deflatemod/.deflatemod.objs/byte \
	-I ${BLD}/extensions/ocsipersist-dbm/.ocsidbmtypes.objs/byte \
	-I ${BLD}/extensions/ocsipersist-dbm/.ocsipersistdbm.objs/byte \
	-I ${BLD}/extensions/ocsipersist-pgsql/.ocsipersistpgsql.objs/byte \
	-I ${BLD}/extensions/ocsipersist-sqlite/.ocsipersistsqlite.objs/byte


OCAMLDOC := ${OCAMLFIND} ocamldoc
ODOC_WIKI := odoc_wiki.cma

LIBS := -package lwt,lwt_log,lwt_ssl,ssl,ipaddr,cohttp-lwt,pcre,xml-light \
	${INCS}

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
