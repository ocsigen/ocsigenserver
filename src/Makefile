include ../Makefile.config

all:
	dune build @all

### CONF ####

###

clean:
	${MAKE} -C baselib clean
	dune clean

distclean:
	${MAKE} -C baselib distclean
	${MAKE} -C http distclean
	${MAKE} -C server distclean
	${MAKE} -C extensions distclean
	-rm -f *~ \#* .\#*
	-cd files; rm -f *~ \#* .\#*
	-rm -fr ../local/var/log/*.log
	-rm -fr ../local/var/lib/ocsidb
