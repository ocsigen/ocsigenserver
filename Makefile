include Makefile.config

### Building

.PHONY: all byte opt doc

all:
	${MAKE} -C src all

byte:
	${MAKE} -C src byte

opt:
	${MAKE} -C src opt

doc:
	$(MAKE) -C doc

### Testing and debugging : local execution and toplevel ###

.PHONY: run.local run.opt.local top

run.local: byte
	CAML_LD_LIBRARY_PATH=src/server src/server/${PROJECTNAME} -c local/etc/${PROJECTNAME}.conf

run.opt.local: opt
	CAML_LD_LIBRARY_PATH=src/server src/server/${PROJECTNAME}.opt -c local/etc/${PROJECTNAME}.conf

top:
	cd src/server && ${MAKE} top

### Cleaning ###

clean: clean.local
	${MAKE} -C src clean

clean.local:
	-rm -f $(PROJECTNAME)-*.tar.gz

distclean: clean.local
	${MAKE} -C src distclean
	-make -C doc clean
	-rm Makefile.config
	-rm -f *~ \#* .\#*

### Installation ####

.PHONY: install partialinstall reinstall uninstall

install.META:
	$(MAKE) -C src install
install.META.byte:
	$(MAKE) -C src install.byte
install.META.opt:
	$(MAKE) -C src install.opt

reinstall: uninstall install
reinstall.byte: uninstall install.byte
reinstall.opt: uninstall install.opt

install: install.META install.files
	@echo
	@echo "## Run \"make doc\" and \"make install.doc\" to build and install the ocamldoc."

install.byte: install.META.byte install.files
install.opt: install.META.opt install.files

install.files:
	 ## Command pipe
	$(INSTALL) -m 755 -d $(dir $(TEMPROOT)$(COMMANDPIPE))
	[ -p $(TEMPROOT)$(COMMANDPIPE) ] || \
	 { mkfifo $(TEMPROOT)$(COMMANDPIPE); \
	   $(CHMOD) 660 $(TEMPROOT)$(COMMANDPIPE); \
	   $(CHOWN) -R $(OCSIGENUSER):$(OCSIGENGROUP) $(TEMPROOT)$(COMMANDPIPE); }
	 ## Configuration files
	$(INSTALL) -m 755 -d $(TEMPROOT)$(CONFIGDIR)/conf.d
	${INSTALL} -m 644 ${PROJECTNAME}.conf.sample $(TEMPROOT)$(CONFIGDIR)/
	[ -f $(TEMPROOT)$(CONFIGDIR)/$(PROJECTNAME).conf ] || \
	  { $(INSTALL) -m 644 $(PROJECTNAME).conf.sample \
                $(TEMPROOT)$(CONFIGDIR)/$(PROJECTNAME).conf;  }
	-mv $(TEMPROOT)$(CONFIGDIR)/mime.types $(TEMPROOT)$(CONFIGDIR)/mime.types.old
	 ## Log directory
	$(INSTALL) -m 644 src/files/mime.types $(TEMPROOT)$(CONFIGDIR)
	$(INSTALL) -d -m 755 -o ${OCSIGENUSER} -g ${OCSIGENGROUP} \
	  $(TEMPROOT)$(LOGDIR)
	 ## Static files
	$(INSTALL) -d -m 755 -o ${OCSIGENUSER} -g ${OCSIGENGROUP} \
	  $(TEMPROOT)$(STATICPAGESDIR)
	$(INSTALL) -d -m 750 -o ${OCSIGENUSER} -g ${OCSIGENGROUP} \
	  $(TEMPROOT)$(DATADIR)
	$(INSTALL) -m 644 -o ${OCSIGENUSER} -g ${OCSIGENGROUP} \
	  local/var/www/*.html $(TEMPROOT)$(STATICPAGESDIR)
	$(INSTALL) -d -m 755 -o ${OCSIGENUSER} -g ${OCSIGENGROUP} \
	  $(TEMPROOT)$(STATICPAGESDIR)/ocsigenstuff
	$(INSTALL) -m 644 -o ${OCSIGENUSER} -g ${OCSIGENGROUP} \
	  local/var/www/ocsigenstuff/* $(TEMPROOT)$(STATICPAGESDIR)/ocsigenstuff
	$(INSTALL) -d -m 755 $(TEMPROOT)$(MANDIR)
	$(INSTALL) -m 644 src/files/${PROJECTNAME}.1 $(TEMPROOT)$(MANDIR)

uninstall:
	-make -C doc uninstall
	-rm -f $(TEMPROOT)$(CONFIGDIR)/$(PROJECTNAME).conf.sample
	-rm -f $(TEMPROOT)$(MANDIR)/${PROJECTNAME}.1
	-rm -f $(TEMPROOT)$(COMMANDPIPE)
	-rmdir --ignore-fail-on-non-empty $(TEMPROOT)$(CONFIGDIR)/conf.d
	-rmdir --ignore-fail-on-non-empty $(TEMPROOT)$(CONFIGDIR)
	-rmdir --ignore-fail-on-non-empty $(TEMPROOT)$(LOGDIR)
	-rmdir --ignore-fail-on-non-empty $(TEMPROOT)$(DATADIR)
	-rmdir --ignore-fail-on-non-empty $(TEMPROOT)$(MANDIR)
	-$(MAKE) -C src uninstall

install.doc:
	${MAKE} -C doc install

### Install logrotate configuration files ###

.PHONY: logrotate

logrotate:
	 $(INSTALL) -m 755 -d $(TEMPROOT)/etc/logrotate.d
	 cat src/files/logrotate.in \
	     | sed s%LOGDIR%$(LOGDIR)%g \
	     | sed s%USER%$(OCSIGENUSER)%g \
	     | sed s%GROUP%$(OCSIGENGROUP)%g \
	     | sed s%_COMMANDPIPE_%$(COMMANDPIPE)%g \
	     > $(TEMPROOT)/etc/logrotate.d/$(PROJECTNAME); }

###

.PHONY: dist

VERSION := $(shell head -n 1 VERSION)
dist:
	DARCS_REPO=$(PWD) darcs dist -d $(PROJECTNAME)-$(VERSION)

###

.PHONY: depend
depend:
	${MAKE} -C src depend