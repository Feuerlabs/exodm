#Test suite for ExoDM#

__Author:__ Ulf Wiger ([`ulf@feuerlabs.com`](mailto:ulf@feuerlabs.com)).

##Running the test suite##

The test suite uses EUnit for now:

```
make test
```

or

```
rebar skip_deps=true eunit
```

A test system is created using `devrun -name exodm_n1`, with the files
located under `.eunit/exodm_tmp/`. This directory is scratched before each
test run, but remains after the completed test run. This makes it possible
to go down to the directory and manually start the node, in order to inspect
and debug interactively.

```
cd .eunit/exodm_tmp
../../devrun -name exodm_n1
```

##Dependencies##

The test suite uses the `parse_trans_codegen` module from
[the `parse_trans` application](http://github.com/esl/parse_trans). This allows
for creating whole sequences that run interpreted (via `erl_eval`) on the
target node.


