
RELNAME=exodm

.PHONY: all compile clean release upgrade

all: compile

compile:
	rebar get-deps
	rebar compile

recompile:
	rebar compile

release: realclean compile
	cd rel; rebar create-node nodeid=${RELNAME}

generate:
	rebar generate skip_deps=true

save_release:
	cp -r rel/${RELNAME} rel/${RELNAME}-${RELVSN}

upgrade:
	rebar generate-appup

clean:
	rebar clean

realclean:
	rebar delete-deps
	rm -rf rel/files rel/${RELNAME}*