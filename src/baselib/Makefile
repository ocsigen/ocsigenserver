include ../../Makefile.config

PREDEP := ocsigen_config_static.ml dynlink_wrapper.ml

## Ocsigen_config ##

ifeq "$(NATDYNLINK)" "YES"
  NATIVECODE_RUNTIME_DETECT=Dynlink.is_native
else
  NATIVECODE_RUNTIME_DETECT=false
endif

VERSION := $(shell head -n 1 ../../VERSION)

ocsigen_config_static.ml: ocsigen_config_static.ml.in ../../Makefile.config ../../Makefile.options ../../VERSION
	cat ocsigen_config_static.ml.in \
	| sed s%_VERSION_%${VERSION}% \
	| sed s%_WARNING_%"Warning: this file has been generated from ocsigen_config_static.ml.in - DO NOT MODIFY MANUALLY!"% \
	| sed s%_LOGDIR_%$(LOGDIR)% \
	| sed s%_DATADIR_%$(DATADIR)%g \
	| sed s%_BINDIR_%$(BINDIR)%g \
	| sed s%_EXTDIR_%$(LIBDIR)/${PROJECTNAME}/extensions%g \
	| sed s%_STATICPAGESDIR_%$(STATICPAGESDIR)% \
	| sed s%_UP_%$(UPLOADDIR)%g \
	| sed s%_OCSIGENUSER_%$(OCSIGENUSER)%g \
	| sed s%_OCSIGENGROUP_%"$(OCSIGENGROUP)"%g \
	| sed s%_PROJECTNAME_%$(PROJECTNAME)%g \
	| sed s%_COMMANDPIPE_%$(COMMANDPIPE)%g \
	| sed s%_CONFIGDIR_%$(CONFIGDIR)% \
	| sed s%_ISNATIVE_%$(NATIVECODE_RUNTIME_DETECT)%g \
	| sed "s%_DEPS_%$(INITPACKAGE)%g" \
	> ocsigen_config_static.ml

## Dynlink_wrapper ##

ifeq "$(NATDYNLINK)" "YES"
dynlink_wrapper.ml:
	ln -s dynlink_wrapper.natdynlink.ml $@
dynlink_wrapper.cmo dynlink_wrapper.cmx: dynlink_wrapper.natdynlink.ml
else
dynlink_wrapper.ml:
	ln -s dynlink_wrapper.nonatdynlink.ml $@
dynlink_wrapper.cmo dynlink_wrapper.cmx: dynlink_wrapper.nonatdynlink.ml
endif

clean:
	-rm -f ${PREDEP}

distclean: clean
	-rm -f *~ \#* .\#*
