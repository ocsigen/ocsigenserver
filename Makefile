include Makefile.config

### Building

.PHONY: all byte opt doc

all:
	${MAKE} -C src all
	dune build

doc:
	$(MAKE) -C doc

### Testing and debugging : local execution and toplevel ###

.PHONY: run.local run.opt.local top

run.local: all
	CAML_LD_LIBRARY_PATH=src/server:$(CAML_LD_LIBRARY_PATH) src/server/${PROJECTNAME} -c local/etc/${PROJECTNAME}.conf

run.opt.local: all
	CAML_LD_LIBRARY_PATH=src/server:$(CAML_LD_LIBRARY_PATH) src/server/${PROJECTNAME}.opt -c local/etc/${PROJECTNAME}.conf

### Cleaning ###

clean: clean.local
	${MAKE} -C src clean
	dune clean

clean.local:
	-rm -f $(PROJECTNAME)-*.tar.gz

distclean: clean.local
	${MAKE} -C src distclean
	-make -C doc clean
	-rm Makefile.config
	-rm -f *~ \#* .\#*

### Installation ####

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
	     > $(TEMPROOT)/etc/logrotate.d/$(PROJECTNAME)

###

.PHONY: depend
depend:
	${MAKE} -C src depend
