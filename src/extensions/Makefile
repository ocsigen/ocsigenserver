include ../../Makefile.config

.PHONY: default
default: build

.PHONY: build
build::
	dune build

ifeq "$(CAMLZIP)" "YES"
build::
	$(MAKE) -C deflatemod
endif

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

clean:
	dune clean
	${MAKE} -C ocsipersist-dbm clean
	${MAKE} -C ocsipersist-sqlite clean
	${MAKE} -C ocsipersist-pgsql clean
	${MAKE} -C deflatemod clean

distclean: clean
	-rm -f *~ \#* .\#*
	${MAKE} -C ocsipersist-dbm distclean
	${MAKE} -C ocsipersist-sqlite distclean
	${MAKE} -C ocsipersist-pgsql distclean
	${MAKE} -C deflatemod distclean
