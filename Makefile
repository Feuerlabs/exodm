
RELNAME=exodm

.PHONY: all compile clean release upgrade

all: compile

tar:
	./exorel tar `./exorel current`

compile:
	./rebar get-deps
	./rebar compile

recompile:
	./rebar compile

release: compile
	cd rel; ../rebar create-node skip_deps=true nodeid=$(RELNAME)

generate:
	(cd rel && ../rebar generate -f skip_deps=true)
	./exorel current `./exorel last_build`

dev:
	./devsetup

devrun:
ifdef n
	./devrun -name $(n1)
else
	echo "no node given (e.g. n=foo make devrun)"
	$(error, no node given)
	exit 2
endif

node:
ifdef n
	./make_node -target nodes/$(n) -rel rel/$(RELNAME) -- -name $(n)
else
	echo "no node given (e.g. n=foo make node)"
	$(error, no node given)
	exit 2
endif

start:
ifdef n
	(cd nodes/$(n); ../../rel/$(RELNAME)/bin/$(RELNAME) start)
else
	echo "no node given (e.g. n=foo make start)"
	$(error, no node given)
	exit 2
endif

attach:
ifdef n
	(cd nodes/$(n); ../../rel/$(RELNAME)/bin/$(RELNAME) attach)
else
	echo "no node given (e.g. n=foo make attach)"
	$(error, no node given)
	exit 2
endif

stop:
ifdef n
	(cd nodes/$(n); ../../rel/$(RELNAME)/bin/$(RELNAME) stop)
else
	echo "no node given (e.g. n=foo make stop)"
	$(error, no node given)
	exit 2
endif

setup:
ifdef n
	(cd nodes/$(n); ../../rel/$(RELNAME)/bin/$(RELNAME) \
	console_boot $(RELNAME)_setup -- -setup is_first false)
else
	echo "no node given (e.g. n=foo make setup)"
	$(error, no node given)
	exit 2
endif


console:
ifdef n
	(cd nodes/$(n); ../../rel/$(RELNAME)/bin/$(RELNAME) console)
else
	echo "no node given (e.g. n=foo make attach)"
	$(error, no node given)
	exit 2
endif


save_release:
ifdef RELVSN
	cp -r rel/$(RELNAME) rel/$(RELNAME)-$(RELVSN)
else
	echo "no RELVSN set"
	$(error no RELVSN set)
	exit 2
endif

upgrade:
ifneq ($(strip $(PREV)),)
	./rebar generate-appup previous_version=$(PREV)
else
	echo "no PREV set"
	exit 2
endif

test: release
	rebar skip_deps=true eunit

clean:
	./rebar clean

realclean:
	rm -f rel/reltool.config
	rm -rf rel/files