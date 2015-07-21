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
	CAML_LD_LIBRARY_PATH=src/server:$(CAML_LD_LIBRARY_PATH) src/server/${PROJECTNAME} -c local/etc/${PROJECTNAME}.conf

run.opt.local: opt
	CAML_LD_LIBRARY_PATH=src/server:$(CAML_LD_LIBRARY_PATH) src/server/${PROJECTNAME}.opt -c local/etc/${PROJECTNAME}.conf

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

.PHONY: install partialinstall reinstall uninstall purge.files install.files

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

# BB If install is not run by root but OCSIGENUSER, OCSIGENGROUP is somebody
# BB different, make files universally accessible, we cannot chown.
INSTALL_CAN_PUT_PERMISSIONS=yes
INSTALL_USER_GROUP=-o $(OCSIGENUSER) -g $(OCSIGENGROUP)
INSTALL_MOD_660=660
INSTALL_MOD_644=644
INSTALL_MOD_755=755
INSTALL_MOD_770=770
INSTALL_MOD_750=750
ifneq ($(shell id -u), 0)
  ifneq ($(OCSIGENUSER), $(USERNAME))
    INSTALL_CAN_PUT_PERMISSIONS=no
  endif
  ifneq ($(shell groups ${USERNAME}|grep -q ${OCSIGENGROUP}; echo $$?), 0)
    INSTALL_CAN_PUT_PERMISSIONS=no
  endif
endif
ifeq ($(INSTALL_CAN_PUT_PERMISSIONS), no)
    INSTALL_USER_GROUP=
    INSTALL_MOD_660=666
    INSTALL_MOD_644=666
    INSTALL_MOD_755=777
    INSTALL_MOD_770=777
    INSTALL_MOD_750=777
endif

install.files:
	@echo INSTALL_CAN_PUT_PERMISSIONS: ${INSTALL_CAN_PUT_PERMISSIONS}
	 ## Command pipe
	$(INSTALL) -m ${INSTALL_MOD_755} -d $(dir $(TEMPROOT)$(COMMANDPIPE))
	[ -p $(TEMPROOT)$(COMMANDPIPE) ] || \
	 { mkfifo -m ${INSTALL_MOD_660} $(TEMPROOT)$(COMMANDPIPE); \
	   if [ "${INSTALL_CAN_PUT_PERMISSIONS}" = yes ]; \
	     then $(CHOWN) -R $(OCSIGENUSER):$(OCSIGENGROUP) $(TEMPROOT)$(COMMANDPIPE); \
	   fi; }
	 ## Configuration files
	$(INSTALL) -m ${INSTALL_MOD_755} -d $(TEMPROOT)$(CONFIGDIR)/conf.d
	${INSTALL} -m ${INSTALL_MOD_644} ${PROJECTNAME}.conf.sample $(TEMPROOT)$(CONFIGDIR)/
	[ -f $(TEMPROOT)$(CONFIGDIR)/$(PROJECTNAME).conf ] || \
	  { $(INSTALL) -m ${INSTALL_MOD_644} $(PROJECTNAME).conf.sample \
                $(TEMPROOT)$(CONFIGDIR)/$(PROJECTNAME).conf;  }
	-mv $(TEMPROOT)$(CONFIGDIR)/mime.types $(TEMPROOT)$(CONFIGDIR)/mime.types.old
	 ## Log directory
	$(INSTALL) -m ${INSTALL_MOD_644} src/files/mime.types $(TEMPROOT)$(CONFIGDIR)
	$(INSTALL) -d -m ${INSTALL_MOD_755} ${INSTALL_USER_GROUP} $(TEMPROOT)$(LOGDIR)
	 ## Static files
	$(INSTALL) -d -m ${INSTALL_MOD_755} ${INSTALL_USER_GROUP} $(TEMPROOT)$(STATICPAGESDIR)
	$(INSTALL) -d -m ${INSTALL_MOD_750} ${INSTALL_USER_GROUP} $(TEMPROOT)$(DATADIR)
	$(INSTALL) -m ${INSTALL_MOD_644} ${INSTALL_USER_GROUP} \
	  local/var/www/*.html $(TEMPROOT)$(STATICPAGESDIR)
	$(INSTALL) -d -m ${INSTALL_MOD_755} ${INSTALL_USER_GROUP} \
	  $(TEMPROOT)$(STATICPAGESDIR)/ocsigenstuff
	$(INSTALL) -m ${INSTALL_MOD_644} ${INSTALL_USER_GROUP} \
	  local/var/www/ocsigenstuff/*.png local/var/www/ocsigenstuff/*.css \
	  $(TEMPROOT)$(STATICPAGESDIR)/ocsigenstuff
	$(INSTALL) -d -m ${INSTALL_MOD_755} $(TEMPROOT)$(MANDIR)
	$(INSTALL) -m ${INSTALL_MOD_644} src/files/${PROJECTNAME}.1 $(TEMPROOT)$(MANDIR)

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

purge: purge.files uninstall

purge.files:
	-rm -f $(TEMPROOT)$(CONFIGDIR)/mime.types $(TEMPROOT)$(CONFIGDIR)/mime.types.old
	-rm -f $(TEMPROOT)$(CONFIGDIR)/$(PROJECTNAME).conf
	-rm -f $(patsubst local/var/www/ocsigenstuff/%, \
	 		  $(TEMPROOT)$(STATICPAGESDIR)/ocsigenstuff/%, \
	                  $(wildcard local/var/www/ocsigenstuff/*))
	-rmdir --ignore-fail-on-non-empty $(TEMPROOT)$(STATICPAGESDIR)/ocsigenstuff
	-rm -f $(patsubst local/var/www/%, \
	 		  $(TEMPROOT)$(STATICPAGESDIR)/%, \
	                  $(wildcard local/var/www/*.html))
	-rmdir --ignore-fail-on-non-empty $(TEMPROOT)$(STATICPAGESDIR)

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
	     > $(TEMPROOT)/etc/logrotate.d/$(PROJECTNAME)

###

.PHONY: depend
depend:
	${MAKE} -C src depend
