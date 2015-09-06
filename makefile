all: compile test

compile: deps
	./rebar compile

deps:
	./rebar get-deps

test:
	ct_run -dir ./test -logdir ./testlogs/ -pa deps/*/ebin/ -pa ebin/

clean:
	rm -vrf deps/ ebin/ erl_crash.dump

.PHONY: all test clean
