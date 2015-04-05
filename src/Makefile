include ../Makefile.config

all: metas confs
	${MAKE} -C baselib all
	${MAKE} -C http all
	${MAKE} -C server all
	${MAKE} -C extensions all

byte: metas confs
	${MAKE} -C baselib byte
	${MAKE} -C http byte
	${MAKE} -C server byte
	${MAKE} -C extensions byte

opt: metas confs
	${MAKE} -C baselib opt
	${MAKE} -C http opt
	${MAKE} -C server opt
	${MAKE} -C extensions opt

### META ###

metas: files/META files/META.${PROJECTNAME}

files/META: files/META.in ../Makefile.config ../Makefile.options Makefile
	cat $< \
	  | sed s/%%CAMLZIPNAME%%/$(CAMLZIPNAME)/ \
	  | sed s/%%NAME%%/$(PROJECTNAME)/g \
	  | sed s/%%DEPS%%/$(shell ${OCAMLFIND} query -p-format -separator ',' ${SERVER_PACKAGE})/g \
	  | sed s/%%BASEDEPS%%/$(shell ${OCAMLFIND} query -p-format -separator ',' ${BASE_PACKAGE})/g \
	  > $@

files/META.${PROJECTNAME}: files/META.in ../Makefile.config ../Makefile.options Makefile
	echo directory = \"../server\" > $@
	cat $< \
	  | sed s/%%CAMLZIPNAME%%/$(CAMLZIPNAME)/ \
	  | sed s/%%NAME%%/$(PROJECTNAME)/g \
	  | sed s/%%DEPS%%/$(shell ${OCAMLFIND} query -p-format -separator ',' ${SERVER_PACKAGE})/g \
	  | sed s/%%BASEDEPS%%/$(shell ${OCAMLFIND} query -p-format -separator ',' ${BASE_PACKAGE})/g \
	  | sed "s%package \"\(polytables\|commandline\|baselib\)\" (%package \"\1\" (\n  directory = \"../baselib\"%" \
	  | sed "s%package \"\(http\|cookies\)\" (%package \"\1\" (\n  directory = \"../http\"%" \
	  | sed "s%directory = \"extensions\"%directory = \"../extensions\"%" \
	  >> $@

### CONF ####

confs: ../$(PROJECTNAME).conf.sample ../local/etc/${PROJECTNAME}.conf

../$(PROJECTNAME).conf.sample: files/$(PROJECTNAME).conf.in ../Makefile.config Makefile
	cat $< \
	  | sed s%_LOGDIR_%$(LOGDIR)%g \
	  | sed s%_DATADIR_%$(DATADIR)%g \
	  | sed s%_OCSIGENUSER_%$(OCSIGENUSER)%g \
	  | sed s%_OCSIGENGROUP_%$(OCSIGENGROUP)%g \
	  | sed s%_COMMANDPIPE_%$(COMMANDPIPE)%g \
	  | sed s%_MIMEFILE_%$(CONFIGDIR)/mime.types%g \
	  | sed s%_METADIR_%$(LIBDIR)%g \
	  | sed s%_PROJECTNAME_%$(PROJECTNAME)%g \
	  | sed s%_LIBDIR_%$(LIBDIR)%g \
	  | sed s%_EXTDIR_%$(LIBDIR)/ocsigenserver/extensions%g \
	  | sed s%_CONFIGDIR_%$(CONFIGDIR)%g \
	  | sed s%_STATICPAGESDIR_%$(STATICPAGESDIR)%g \
	  | sed s%_EXTPACKAGENAME_%$(PROJECTNAME).ext%g \
	  > $@

../local/etc/$(PROJECTNAME).conf: files/${PROJECTNAME}.conf.in ../Makefile.config Makefile
	mkdir -p ../local/etc ../local/var/log ../local/var/run
	cat $< \
	  | sed s%80\</port\>%8080\</port\>%g \
	  | sed s%_LOGDIR_%$(SRC)/local/var/log%g \
	  | sed s%_DATADIR_%$(SRC)/local/var/lib%g \
	  | sed s%\<user\>_OCSIGENUSER_\</user\>%%g \
	  | sed s%\<group\>_OCSIGENGROUP_\</group\>%%g \
	  | sed s%_COMMANDPIPE_%$(SRC)/local/var/run/${PROJECTNAME}_command%g \
	  | sed s%_MIMEFILE_%$(SRC)/src/files/mime.types%g \
	  | sed s%_METADIR_%${LIBDIR}\"/\>\<findlib\ path=\"$(SRC)/src/files/\"/\>\<findlib\ path=\"$(SRC)/src/extensions/files/%g \
	  | sed s%_PROJECTNAME_%$(PROJECTNAME)%g \
	  | sed s%_EXTLIBDIR_%${SRC}/src/extensions/ocsipersist-dbm%g \
	  | sed s%_CONFIGDIR_%$(SRC)/local/etc%g \
	  | sed s%_STATICPAGESDIR_%$(SRC)/local/var/www%g \
	  | sed s%_EXTPACKAGENAME_%$(PROJECTNAME).ext%g \
	  | sed s%\<\!--\ \<commandpipe%\<commandpipe%g \
	  | sed s%\</commandpipe\>\ --\>%\</commandpipe\>%g \
	  | sed s%\<\!--\ \<mimefile%\<mimefile%g \
	  | sed s%\</mimefile\>\ --\>%\</mimefile\>%g \
	  | sed s%store\ dir=\"$(SRC)/var/lib\"%store\ dir=\"$(SRC)/local/var/lib/ocsipersist\"%g \
	  > $@


### Install ###

include Makefile.filelist

VERSION := $(shell head -n 1 ../VERSION)

install: INSTALL_BIN=$(BIN) $(NATBIN)
install: INSTALL_IMPL=$(IMPL) $(NATIMPL)
install: INSTALL_PLUGINS_IMPL=$(PLUGINS_IMPL) $(PLUGINS_NATIMPL)
install: INSTALL_PLUGINS_BIN=$(PLUGINS_BIN) $(PLUGINS_NATBIN)
install: raw_install

install.byte: INSTALL_BIN=$(BIN)
install.byte: INSTALL_IMPL=$(IMPL)
install.byte: INSTALL_PLUGINS_IMPL=$(PLUGINS_IMPL)
install.byte: INSTALL_PLUGINS_BIN=$(PLUGINS_BIN)
install.byte: raw_install

install.opt: INSTALL_BIN=$(NATBIN)
install.opt: INSTALL_IMPL=$(NATIMPL)
install.opt: INSTALL_PLUGINS_IMPL=$(PLUGINS_NATIMPL)
install.opt: INSTALL_PLUGINS_BIN=$(PLUGINS_NATBIN)
install.opt: raw_install

raw_install:
	 # Server binaries
	$(INSTALL) -m 755 -d $(TEMPROOT)$(BINDIR)
	$(INSTALL) -m 755 $(INSTALL_BIN) $(TEMPROOT)$(BINDIR)/
	 # Server libraries
	$(INSTALL) -m 755 -d $(TEMPROOT)$(LIBDIR)
	$(OCAMLFIND) install $(PROJECTNAME) \
	   -destdir $(TEMPROOT)$(LIBDIR) \
	   -patch-version ${VERSION} \
	   ${INTF} $(INSTALL_IMPL) $(DOC) files/META
	 # Extensions
	$(INSTALL) -m 755 -d $(TEMPROOT)${LIBDIR}/${PROJECTNAME}/extensions/
	$(INSTALL) -m 644 \
	  $(PLUGINS_INTF) $(INSTALL_PLUGINS_IMPL) $(PLUGINS_DOC) \
	  $(TEMPROOT)${LIBDIR}/${PROJECTNAME}/extensions/
	[ -z "$(strip ${INSTALL_PLUGINS_BIN})" ] || \
	  $(INSTALL) -m 755 $(INSTALL_PLUGINS_BIN) \
	  $(TEMPROOT)${LIBDIR}/${PROJECTNAME}/extensions/

uninstall:
	 # Extensions
	-rm -r $(TEMPROOT)${LIBDIR}/${PROJECTNAME}/extensions/
	 # Server libraries
	-$(OCAMLFIND) remove ${PROJECTNAME} -destdir $(TEMPROOT)${LIBDIR}
	 # Server binaries
	-rm -r $(addprefix $(TEMPROOT)$(BINDIR)/,$(notdir ${BIN} ${NATBIN}))
	-rmdir --ignore-fail-on-non-empty $(TEMPROOT)$(BINDIR)

reinstall: uninstall install

###

clean: clean.local
	${MAKE} -C baselib clean
	${MAKE} -C http clean
	${MAKE} -C server clean
	${MAKE} -C extensions clean

clean.local:
	-rm -f ../${PROJECTNAME}.conf.sample
	-rm -f ../${PROJECTNAME}.conf.opt.sample
	-rm -f ../${PROJECTNAME}.conf.local
	-rm -f ../${PROJECTNAME}.conf.opt.local
	-rm -f files/META files/META.${PROJECTNAME}

distclean: clean.local
	${MAKE} -C baselib distclean
	${MAKE} -C http distclean
	${MAKE} -C server distclean
	${MAKE} -C extensions distclean
	-rm -f *~ \#* .\#*
	-cd files; rm -f *~ \#* .\#*
	-rm -fr ../local/var/log/*.log
	-rm -fr ../local/var/lib/ocsidb
	-rm -fr ../local/etc/${PROJECTNAME}.conf
	-rm -fr ../local/etc/${PROJECTNAME}.conf.opt

depend:
	${MAKE} -C baselib depend
	${MAKE} -C http depend
	${MAKE} -C server depend
	${MAKE} -C extensions depend
