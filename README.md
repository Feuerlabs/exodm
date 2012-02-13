This is a skeleton project for rebar-based packaging and release handling

A good starting point for understanding rebar and reltool is

http://www.metabrew.com/article/erlang-rebar-tutorial-generating-releases-upgrades

The idea is to use rebar dependencies to list which applications are to be part of the
system. They then need to be added to rel/reltool.config as well.

The make target `make release` followed by `make generate` will create a set of
files needed to boot a node.

The function of each make step is as follows:

## make

Fetches dependencies and compiles all sources.

## make release

Collects information for OTP's RelTool so that a release can be generated.
This step needs to be run if new applications are added or removed, or changes
are made in the `exodm/rel/reltool.config.src` file

make generate
-------------

Copies code to the release structure and builds boot scripts.
You need to run `make generate` again, whenever you have changed code, or the contents
of .app files, or files in some application's `priv/` directory.

make node
---------

In order to run multiple nodes, a special structure has been set up to allow each
node its own diretory for storing logs, databases, etc., as well as setting node-specific
parameters. The `make node` command needs a node name, which is given like so:

`n=n1 make node`

This will create a directory, `exodm/nodes/n1`, with files necessary to start the node `n1`.

make console | start | attach | stop
------------------------------------

If you want to start a node with an interactive shell, use the command:

`n=n1 make console`

This will `cd` into the `nodes/n1` directory and start an erlang node, with the proper
boot script and settings.

`n=n1 make start` will start a node in "embedded mode", which can be connected to using
`n=n1 make attach`. The corresponding `make stop` will terminate the node.

(or `exodm start` followed by `exodm attach` when running embedded).

TODO
====

There is a `setup` target as well, for starting nodes in a special installation phase.
This is not yet used. One problem is figuring out how to set up multiple nodes. An idea is
to always start with a single-node setup, then bootstrap new nodes one by one.

Upgrade targets are likewise not yet operational.