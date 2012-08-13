-module(exodm_rpc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    exodm_rpc_sup:start_link().

stop(_State) ->
    ok.

%% start_phase(init, _, []) ->
%%     exodm_rpc:init().
%% start_phase(load_specs, _, []) ->
%%     %% exodm_rpc_spec:load_specs(),
%%     ok.
