DEPS=./deps
HDF5_VERSION := $(shell curl http://www.hdfgroup.org/ftp/HDF5/current/src/ | grep '<title>.*</title>' | awk '{print $$2}')
HDF5_FLAGS=

TEST_SUPPORT = \
	test/etap.beam

%.beam: %.erl
	erlc -o test/ $<

all:$(DEPS)/hdf5/lib/libhdf5.so
	./rebar compile

$(DEPS)/hdf5:
	@mkdir -p $(DEPS)/hdf5; cd $(DEPS) ; \
	curl http://www.hdfgroup.org/ftp/HDF5/current/src/hdf5-$(HDF5_VERSION).tar.gz | tar xzf -

$(DEPS)/hdf5/lib/libhdf5.so: $(DEPS)/hdf5
	@cd $(DEPS)/hdf5-$(HDF5_VERSION) && CFLAGS=-O1 ./configure --prefix $(CURDIR)/$(DEPS)/hdf5  \
	$(HDF5_FLAGS) && make && make install

# check: $(TEST_SUPPORT)
# 	prove test/*.t

etags:
	find . | grep ".*\.\(h\|hxx\|c\)" | xargs etags -f TAGS

clean:
	rebar clean
	rm -rf test/*.beam

distclean:
	rm -rf $(DEPS)
	rm -rf priv

test: all
	mkdir -p .eunit
	cp -r priv .eunit/.
	./rebar skip_deps=true eunit
	./rebar ct
