LINUX=$(shell uname | grep Linux | wc -l | xargs echo)
DEPS=../deps

ERLANG_HOME=/usr/local/lib/erlang
HDF5_FLAGS=

HDF5_VERSION := $(shell curl http://www.hdfgroup.org/ftp/HDF5/current/src/ | grep '<title>.*</title>' | awk '{print $$2}')

all: $(DEPS)/hdf5/lib/libhdf5.la

$(DEPS)/hdf5:
	@mkdir $(DEPS); cd $(DEPS) ; \
	curl http://www.hdfgroup.org/ftp/HDF5/current/src/hdf5-$(HDF5_VERSION).tar.gz | tar xzf -

$(DEPS)/hdf5/lib/libhdf5.la: $(DEPS)/hdf5
	@cd $(DEPS)/hdf5-$(HDF5_VERSION) && ./configure --prefix $(CURDIR)/$(DEPS)/hdf5  \
	$(HDF5_FLAGS) && make && make install
