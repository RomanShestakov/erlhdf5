OS:=$(shell uname -s)
DEPS=./deps
ARCH:= $(shell uname -p)
HDF5_VERSION := $(shell curl http://www.hdfgroup.org/ftp/HDF5/current/src/ | grep '<title>.*</title>' | awk '{print $$2}')
HDF5_FLAGS=
CC:=

DRV_CC_TEMPLATE:=
DRV_LINK_TEMPLATE:=
EXE_LINK_TEMPLATE:=
EXE_CC_TEMPLATE:=
LDFLAGS:=

ifeq ($(OS),Darwin)
LIBRARY=$(DEPS)/hdf5/lib/libhdf5.dylib
else
LIBRARY=$(DEPS)/hdf5/lib/libhdf5.so
endif

ifeq ($(ARCH), armv7l)
ERLCFLAGS := -g -Wall -fPIC  -I/usr/lib/erlang/lib/erl_interface-3.7.3/include -I/usr/lib/erlang/erts-5.8.3/include -DH5_NO_DEPRECATED_SYMBOLS
H5CCBASE := $(shell h5cc --version| head -1| awk '{print $$1}')
H5CC := $(shell which h5cc)
else
ERLCFLAGS := "-g -Wall -fPIC  -I/usr/lib/erlang/lib/erl_interface-3.7.9/include -I/usr/lib/erlang/erts-5.9.3.1/include"
H5CCBASE :=
H5CC:=deps/hdf5/bin/h5cc
endif

TEST_SUPPORT = \
	test/etap.beam

%.beam: %.erl
	erlc -o test/ $<

all:$(LIBRARY)
	CC=$(H5CC) CFLAGS=$(ERLCFLAGS) ./rebar compile

hdf5: $(LIBRARY)

$(DEPS)/hdf5:
ifneq ($(H5CCBASE), gcc)
	@mkdir -p $(DEPS)/hdf5; cd $(DEPS) ; \
	curl http://www.hdfgroup.org/ftp/HDF5/current/src/hdf5-$(HDF5_VERSION).tar.gz | tar xzf -
else
	@mkdir -p $(DEPS)/hdf5; cd $(DEPS) ; 
endif


$(LIBRARY): $(DEPS)/hdf5
ifneq ($(H5CCBASE), gcc)
	@cd $(DEPS)/hdf5-$(HDF5_VERSION) && CFLAGS=-O1 ./configure --prefix $(CURDIR)/$(DEPS)/hdf5  \
	$(HDF5_FLAGS) && make && make install
endif

# check: $(TEST_SUPPORT)
# 	prove test/*.t

etags:
	find . | grep ".*\.\(h\|hxx\|c\)" | xargs etags -f TAGS

clean:
	./rebar clean
	rm -rf test/*.beam

distclean:
	rm -rf $(DEPS)
	rm -rf priv

test: all
	mkdir -p .eunit
	cp -r priv .eunit/.
	./rebar skip_deps=true eunit
	./rebar ct
