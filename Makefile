
RELNAME=exodm

.PHONY: all compile clean release upgrade

all: compile

compile:
	rebar get-deps
	rebar compile

recompile:
	rebar compile

release: realclean compile
	cd rel; rebar create-node nodeid=$(RELNAME)

generate:
	rebar generate skip_deps=true

save_release:
ifneq ($(strip $(RELVSN)),)
	cp -r rel/$(RELNAME) rel/$(RELNAME)-$(RELVSN)
else
	echo "no RELVSN set"
	exit 1
endif

upgrade:
ifneq ($(strip $(PREV)),)
	rebar generate-appup previous_version=$(PREV)
else
	echo "no PREV set"
	exit 1
endif

clean:
	rebar clean

realclean:
	rebar delete-deps
	rm -rf rel/files rel/$(RELNAME)*