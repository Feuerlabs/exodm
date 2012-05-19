%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
-module(exodm_db_app).

-export([start/2, stop/1]).


start(_Type, _Arg) ->
    exodm_db_sup:start_link().

stop(_State) ->
    ok.
