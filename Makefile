include Makefile.options
include Makefile.config

### Building

.PHONY: default all
default all: build

.PHONY: build
build:
	dune build
	${MAKE} -C src/server build
doc:
	$(MAKE) -C doc

### Testing : local execution and toplevel ###

.PHONY: run.local run.opt.local top

run.local: build install.files
	CAML_LD_LIBRARY_PATH=${BLD}/server:$(CAML_LD_LIBRARY_PATH) ${BLD}/ocsigenserver.bc -c local/etc/ocsigenserver.conf

run.opt.local: build install.files
	CAML_LD_LIBRARY_PATH=${BLD}/server:$(CAML_LD_LIBRARY_PATH) ${BLD}/ocsigenserver.exe -c local/etc/ocsigenserver.conf

top:
	cd src/server && ${MAKE} top

### Cleaning ###

clean: clean.local
	${MAKE} -C src/server clean

clean.local:
	dune clean
	-rm -f ocsigenserver-*.tar.gz

distclean: clean.local
	${MAKE} -C src/server distclean
	-make -C doc clean
	-rm Makefile.config
	-rm -f *~ \#* .\#*

### Installation ####

.PHONY: purge.files install.files

# BB If install is not run by root but OCSIGENUSER, OCSIGENGROUP is somebody
# BB different, make files universally accessible, we cannot chown.
INSTALL_CAN_PUT_PERMISSIONS=yes
INSTALL_USER_GROUP=-o $(OCSIGENUSER) -g "$(OCSIGENGROUP)"
INSTALL_MOD_660=660
INSTALL_MOD_644=644
INSTALL_MOD_755=755
INSTALL_MOD_770=770
INSTALL_MOD_750=750
USERNAME=$(shell whoami)
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
	@echo
	@echo "## Run \"make doc\" and \"make install.doc\" to build and install the ocamldoc."
	@echo INSTALL_CAN_PUT_PERMISSIONS: ${INSTALL_CAN_PUT_PERMISSIONS}
	 ## Command pipe
	$(INSTALL) -m ${INSTALL_MOD_755} -d $(dir $(TEMPROOT)$(COMMANDPIPE))
	[ -p $(TEMPROOT)$(COMMANDPIPE) ] || \
	 { mkfifo -m ${INSTALL_MOD_660} $(TEMPROOT)$(COMMANDPIPE); \
	   if [ "${INSTALL_CAN_PUT_PERMISSIONS}" = yes ]; \
	     then $(CHOWN) -R $(OCSIGENUSER):"$(OCSIGENGROUP)" $(TEMPROOT)$(COMMANDPIPE); \
	   fi; }
	 ## Configuration files
	$(INSTALL) -m ${INSTALL_MOD_755} -d $(TEMPROOT)$(CONFIGDIR)/conf.d
	${INSTALL} -m ${INSTALL_MOD_644} ocsigenserver.conf.sample $(TEMPROOT)$(CONFIGDIR)/
	[ -f $(TEMPROOT)$(CONFIGDIR)/ocsigenserver.conf ] || \
	  { $(INSTALL) -m ${INSTALL_MOD_644} ocsigenserver.conf.sample \
		$(TEMPROOT)$(CONFIGDIR)/ocsigenserver.conf;  }
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
	$(INSTALL) -m ${INSTALL_MOD_644} src/files/ocsigenserver.1 $(TEMPROOT)$(MANDIR)

uninstall:
	-make -C doc uninstall
	-rm -f $(TEMPROOT)$(CONFIGDIR)/ocsigenserver.conf.sample
	-rm -f $(TEMPROOT)$(MANDIR)/ocsigenserver.1
	-rm -f $(TEMPROOT)$(COMMANDPIPE)
	-rmdir --ignore-fail-on-non-empty $(TEMPROOT)$(CONFIGDIR)/conf.d
	-rmdir --ignore-fail-on-non-empty $(TEMPROOT)$(CONFIGDIR)
	-rmdir --ignore-fail-on-non-empty $(TEMPROOT)$(LOGDIR)
	-rmdir --ignore-fail-on-non-empty $(TEMPROOT)$(DATADIR)
	-rmdir --ignore-fail-on-non-empty $(TEMPROOT)$(MANDIR)

purge: purge.files

purge.files:
	-rm -f $(TEMPROOT)$(CONFIGDIR)/mime.types $(TEMPROOT)$(CONFIGDIR)/mime.types.old
	-rm -f $(TEMPROOT)$(CONFIGDIR)/ocsigenserver.conf
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
	     | sed s%GROUP%"$(OCSIGENGROUP)"%g \
	     | sed s%_COMMANDPIPE_%$(COMMANDPIPE)%g \
	     > $(TEMPROOT)/etc/logrotate.d/ocsigenserver
