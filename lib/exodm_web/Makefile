default: compile

get-deps:
	rebar get-deps

compile: get-deps
	rebar compile

clean:
	rebar delete-deps
	rebar clean

distclean: clean
	-rm -rf deps ebin
