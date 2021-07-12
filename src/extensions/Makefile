include ../../Makefile.config

.PHONY: default
default: build

.PHONY: build
build::
	dune build

### PostgreSQL ###

ifeq "$(OCSIPERSISTPGSQL)" "YES"
build::
	$(MAKE) -C ocsipersist-pgsql
endif
### SQLite ###

ifeq "$(OCSIPERSISTSQLITE)" "YES"
build::
	$(MAKE) -C ocsipersist-sqlite
endif

### DBM ####

ifeq "$(OCSIPERSISTDBM)" "YES"
build::
	$(MAKE) -C ocsipersist-dbm
endif

## Clean up

distclean: clean
	-rm -f *~ \#* .\#*
	${MAKE} -C ocsipersist-dbm distclean
	${MAKE} -C ocsipersist-sqlite distclean
	${MAKE} -C ocsipersist-pgsql distclean
	${MAKE} -C deflatemod distclean
