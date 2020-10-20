include ../Makefile.config

all: confs
	${MAKE} -C baselib
	${MAKE} -C http
	${MAKE} -C server
	${MAKE} -C extensions
	dune build

### CONF ####

confs: ../$(PROJECTNAME).conf.sample ../local/etc/${PROJECTNAME}.conf

../$(PROJECTNAME).conf.sample: files/$(PROJECTNAME).conf.in ../Makefile.config Makefile
	cat $< \
	  | sed s%_LOGDIR_%$(LOGDIR)%g \
	  | sed s%_DATADIR_%$(DATADIR)%g \
	  | sed s%_OCSIGENUSER_%$(OCSIGENUSER)%g \
	  | sed s%_OCSIGENGROUP_%"$(OCSIGENGROUP)"%g \
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

###

clean: clean.local
	${MAKE} -C baselib clean
	${MAKE} -C http clean
	${MAKE} -C server clean
	${MAKE} -C extensions clean
	dune clean

clean.local:
	-rm -f ../${PROJECTNAME}.conf.sample
	-rm -f ../${PROJECTNAME}.conf.opt.sample
	-rm -f ../${PROJECTNAME}.conf.local
	-rm -f ../${PROJECTNAME}.conf.opt.local

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
