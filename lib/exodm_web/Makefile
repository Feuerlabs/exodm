default: compile static/nitrogen

get-deps:
	rebar get-deps

static/nitrogen:
	ln -sf ../deps/nitrogen_core/www static/nitrogen

compile: get-deps
	rebar compile

clean:
	-rm -f static/nitrogen
	rebar delete-deps
	rebar clean

distclean: clean
	-rm -rf deps ebin
