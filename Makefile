
TEST_SUPPORT = \
	test/etap.beam

%.beam: %.erl
	erlc -o test/ $<

all:
	./rebar compile

# check: $(TEST_SUPPORT)
# 	prove test/*.t

clean:
	rebar clean
	rm -rf test/*.beam

test: all
	mkdir -p .eunit
	cp -r priv .eunit/.
	./rebar skip_deps=true eunit
