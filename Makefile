
TEST_SUPPORT = \
	test/etap.beam

%.beam: %.erl
	erlc -o test/ $<

all:
	./rebar compile

# check: $(TEST_SUPPORT)
# 	prove test/*.t

etags:
	find . | grep ".*\.\(h\|hxx\|c\)" | xargs etags -f TAGS

clean:
	rebar clean
	rm -rf test/*.beam

test: all
	mkdir -p .eunit
	cp -r priv .eunit/.
	./rebar skip_deps=true eunit
