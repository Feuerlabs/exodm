RELNAME=exodm

ESL="$(PWD)/rel/plugins"
EXODM_DIR=$(PWD)
ED=$(EXODM_DIR)/deps
REBAR=$(EXODM_DIR)/rebar

.PHONY: all compile clean release upgrade test node console start attach tar \
	recompile dev devrun

all: compile

tar:
	./exorel tar `./exorel current` -root $(PWD)

compile:
	$(REBAR) get-deps
	$(REBAR) compile

recompile:
	$(REBAR) compile

release: compile
	cd rel; $(REBAR) create-node skip_deps=true nodeid=$(RELNAME)

generate:
	rm -f ./rel/exodm
	rm -f rel/exodm rel/lib/exodm
	(cd rel && $(REBAR) generate -f skip_deps=true)
	./exorel current `./exorel last_build` -root $(PWD)

dev:
	./devsetup -target false

devrun:
ifdef n
	./devrun -name $(n)
else
	echo "no node given (e.g. n=foo make devrun)"
	$(error, no node given)
	exit 2
endif

node:
ifdef n
	./make_node -target $(PWD)/nodes/$(n) -rel $(PWD)/rel/$(RELNAME) -- -name $(n)
else
	echo "no node given (e.g. n=foo make node)"
	$(error, no node given)
	exit 2
endif

start:
ifdef n
	(cd nodes/$(n); ERL_SETUP_LIBS=$(ESL) \
	../../rel/$(RELNAME)/bin/$(RELNAME) start)
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
	(cd nodes/$(n); ERL_SETUP_LIBS=$(ESL) \
	../../rel/$(RELNAME)/bin/$(RELNAME) \
	console_boot $(RELNAME)_setup -- -setup mode setup)
else
	echo "no node given (e.g. n=foo make setup)"
	$(error, no node given)
	exit 2
endif

convert:
ifdef n
	(cd nodes/$(n); ERL_SETUP_LIBS=$(ESL) \
	../../rel/$(RELNAME)/bin/$(RELNAME) \
	console_boot $(RELNAME)_setup -- -setup mode convert)
else
	echo "no node given (e.g. n=foo make setup)"
	$(error, no node given)
	exit 2
endif


console:
ifdef n
	(cd nodes/$(n); ERL_SETUP_LIBS=$(ESL) \
	../../rel/$(RELNAME)/bin/$(RELNAME) console)
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

appup:
ifneq ($(strip $(PREV)),)
	(cd rel; $(REBAR) -vvv generate-appup skip_deps=true previous_release=$(PREV))
else
	echo "no PREV set"
	exit 2
endif

upgrade:
ifneq ($(strip $(PREV)),)
	(cd rel; $(REBAR) -vvv generate-upgrade skip_deps=true previous_release=$(PREV))
else
	echo "no PREV set"
	exit 2
endif

test:
	EXO_TEST=true $(REBAR) skip_deps=true eunit

retest:
	EXO_TEST=true EXODM_SKIP_MAKE=true $(REBAR) skip_deps=true eunit

# `make test_console` steps into the EUnit directory for the test system
# and starts exodm in 'console' mode. This is useful after a `make test`,
# in order to inspect the database, run test commands, etc.
test_console:
	cd .eunit/exodm_tmp; ERL_SETUP_LIBS=$(ESL) ../../rel/exodm/bin/exodm console

# Note: only works if install=false in ck3_test/test/ck3.cfg
# Use this if you don't have REBAR_DEPS, ERL_LIBS and EXODM_PLUGIN_DIR set 
ck3_test:
	CK3_TEST=true $(REBAR) get-deps compile
	rm -rf rel/plugins
	mkdir -p rel/plugins
	ln -s $(PWD)/deps/exodm_ck3 rel/plugins/
	(cd deps/ck3_test/test && ERL_LIBS=$(EXODM_DIR)/deps make)
	echo "starting CT run"
	(cd deps/ck3_test/test && \
	ERL_LIBS=$(ED) ct_run -config ck3.cfg -suite ck3_SUITE -erl_args -name ct -setcookie exodm -pz $(EXODM_DIR)/ebin)
	rm -rf rel/plugins

# Use this if you have REBAR_DEPS, ERL_LIBS and EXODM_PLUGIN_DIR set 
# and all repos cloned
ck3_test_local:
ifdef EXODM_PLUGIN_DIR
	$(REBAR) get-deps compile
	rm -rf rel/plugins
	mkdir -p rel/plugins
	ln -s $(EXODM_PLUGIN_DIR)/exodm_ck3 rel/plugins/
	echo "starting CT run"
	(cd $(ERL_LIBS)/ck3_test/test && \
	 ct_run -config ck3.cfg -suite ck3_SUITE -erl_args -name ct -setcookie exodm)
	rm -rf rel/plugins
else
	echo "no EXODM_PLUGIN_DIR set"
	$(error no EXODM_PLUGIN_DIR set)
	exit 2
endif	

clean:
	$(REBAR) clean

realclean:
	rm -f rel/reltool.config
	rm -rf rel/files