This is a skeleton project for rebar-based packaging and release handling

A good starting point for understanding rebar and reltool is

http://www.metabrew.com/article/erlang-rebar-tutorial-generating-releases-upgrades

The idea is to use rebar dependencies to list which applications are to be part of the
system. They then need to be added to rel/reltool.config as well.

The make target `make release` followed by `make generate` will create a set of
files needed to boot a node. NOTE: `make release` actually cleans away any prior
releases, so run this only when you want a fresh start.

When the bootscript needs to be regenerated, run `make generate` again.

The system can be started using `rel/exodm/(VSN)/bin/exodm console`

(or `exodm start` followed by `exodm attach` when running embedded).

The (VSN) part is constructed from the git tag, and/or the git version of
the current commit.

TODO: ensure that multiple nodes can be started, using different node names,
and becoming aware of each other. This is not something rebar supports out of the box,
I think.

There is a beginning towards support for upgrade scripts, but this hasn't been tested.
Running e.g. `PREV=1.0 make upgrade`, upgrade scripts should be generated for moving from
the named previous release to the current one.