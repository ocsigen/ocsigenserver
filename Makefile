include Makefile.config
include Makefile.options

### Building

.PHONY: default all
default all: build

.PHONY: build
build:
	dune build -p ocsigenserver
	${MAKE} -C src/server build

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
	-rm Makefile.config
	-rm -f *~ \#* .\#*

### Installation ####

.PHONY: purge.files install.files

# BB If install is not run by root but OCSIGENUSER, OCSIGENGROUP is somebody
# BB different, make files universally accessible, we cannot chown.
INSTALL_CAN_PUT_PERMISSIONS=yes
INSTALL_USER_GROUP=-o $(OCSIGENUSER) -g "$(OCSIGENGROUP)"
INSTALL_MOD_660=-m 660
INSTALL_MOD_644=-m 644
INSTALL_MOD_755=-m 755
INSTALL_MOD_770=-m 770
INSTALL_MOD_750=-m 750
INSTALL_DIR=$(INSTALL) -d
INSTALL_FILE=$(INSTALL)
# The Unix permission/ownership handling below is meaningless on Windows (the
# OCSIGEN_WINDOWS branch drops modes and ownership anyway), and OCSIGENGROUP is
# empty there, which makes the `grep` check below print a usage error. Skip it.
ifneq ($(OCSIGEN_WINDOWS), yes)
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
    INSTALL_MOD_660=-m 666
    INSTALL_MOD_644=-m 666
    INSTALL_MOD_755=-m 777
    INSTALL_MOD_770=-m 777
    INSTALL_MOD_750=-m 777
endif
endif

# On Windows (Cygwin), NTFS does not support Unix permission bits and `install`
# aborts when it fails to chmod the freshly created file or directory. Fall back
# to plain `mkdir -p` / `cp` and drop the mode and ownership flags.
# OCSIGEN_WINDOWS is set by ./configure (see Makefile.config).
ifeq ($(OCSIGEN_WINDOWS), yes)
    INSTALL_USER_GROUP=
    INSTALL_MOD_660=
    INSTALL_MOD_644=
    INSTALL_MOD_755=
    INSTALL_MOD_770=
    INSTALL_MOD_750=
    INSTALL_DIR=mkdir -p
    INSTALL_FILE=cp
endif

install.files:
	@echo
	@echo INSTALL_CAN_PUT_PERMISSIONS: ${INSTALL_CAN_PUT_PERMISSIONS}
	 ## Configuration files
	$(INSTALL_DIR) ${INSTALL_MOD_755} $(TEMPROOT)$(CONFIGDIR)/conf.d
	$(INSTALL_FILE) ${INSTALL_MOD_644} _build/install/default/etc/ocsigenserver/ocsigenserver.conf.sample $(TEMPROOT)$(CONFIGDIR)/
	[ -f $(TEMPROOT)$(CONFIGDIR)/ocsigenserver.conf ] || \
	  { $(INSTALL_FILE) ${INSTALL_MOD_644} _build/install/default/etc/ocsigenserver/ocsigenserver.conf.sample \
		$(TEMPROOT)$(CONFIGDIR)/ocsigenserver.conf;  }
	-mv $(TEMPROOT)$(CONFIGDIR)/mime.types $(TEMPROOT)$(CONFIGDIR)/mime.types.old
	 ## Log directory
	$(INSTALL_FILE) ${INSTALL_MOD_644} src/files/mime.types $(TEMPROOT)$(CONFIGDIR)
	$(INSTALL_DIR) ${INSTALL_MOD_755} ${INSTALL_USER_GROUP} $(TEMPROOT)$(LOGDIR)
	 ## Static files
	$(INSTALL_DIR) ${INSTALL_MOD_755} ${INSTALL_USER_GROUP} $(TEMPROOT)$(STATICPAGESDIR)
	$(INSTALL_DIR) ${INSTALL_MOD_750} ${INSTALL_USER_GROUP} $(TEMPROOT)$(DATADIR)
	$(INSTALL_FILE) ${INSTALL_MOD_644} ${INSTALL_USER_GROUP} \
	  local/var/www/*.html $(TEMPROOT)$(STATICPAGESDIR)
	$(INSTALL_DIR) ${INSTALL_MOD_755} ${INSTALL_USER_GROUP} \
	  $(TEMPROOT)$(STATICPAGESDIR)/ocsigenstuff
	$(INSTALL_FILE) ${INSTALL_MOD_644} ${INSTALL_USER_GROUP} \
	  local/var/www/ocsigenstuff/*.png local/var/www/ocsigenstuff/*.css \
	  $(TEMPROOT)$(STATICPAGESDIR)/ocsigenstuff
	mkdir -p $(TEMPROOT)$(MANDIR)
	$(INSTALL_FILE) ${INSTALL_MOD_644} src/files/ocsigenserver.1 $(TEMPROOT)$(MANDIR)

uninstall:
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
