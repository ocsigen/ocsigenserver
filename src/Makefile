include ../Makefile.config

all: confs
	dune build @all

### CONF ####

confs: ../ocsigenserver.conf.sample ../local/etc/ocsigenserver.conf
	${MAKE} -C baselib confs

../ocsigenserver.conf.sample: files/ocsigenserver.conf/options.ml
	dune exec -- files/ocsigenserver.conf/gen.exe > "$@"

../local/etc/ocsigenserver.conf: files/ocsigenserver.conf/options.ml
	mkdir -p ../local/etc ../local/var/log ../local/var/run
	dune exec -- files/ocsigenserver.conf/gen.exe local > "$@"

###

clean: clean.local
	${MAKE} -C baselib clean
	dune clean

clean.local:
	-rm -f ../ocsigenserver.conf.sample
	-rm -f ../ocsigenserver.conf.opt.sample
	-rm -f ../ocsigenserver.conf.local
	-rm -f ../ocsigenserver.conf.opt.local

distclean: clean.local
	${MAKE} -C baselib distclean
	${MAKE} -C http distclean
	${MAKE} -C server distclean
	${MAKE} -C extensions distclean
	-rm -f *~ \#* .\#*
	-cd files; rm -f *~ \#* .\#*
	-rm -fr ../local/var/log/*.log
	-rm -fr ../local/var/lib/ocsidb
	-rm -fr ../local/etc/ocsigenserver.conf
	-rm -fr ../local/etc/ocsigenserver.conf.opt
