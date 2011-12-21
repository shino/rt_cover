.PHONY: all compile deps clean test

EBIN_DIR = ebin

all: deps compile test

compile:
	./rebar compile skip_deps=true
	./rebar xref skip_deps=true

deps:
	./rebar update-deps
	./rebar get-deps
	./rebar compile
	./rebar xref skip_deps=true

clean:
	./rebar clean skip_deps=true
	-rm -f erl_crash.dump
	-rm -f TEST-*.xml

test:
	./rebar eunit skip_deps=true
