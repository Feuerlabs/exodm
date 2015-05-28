-module(exodm_rpc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start_phase/3]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    exodm_rpc_sup:start_link().

stop(_State) ->
    ok.

start_phase(init, _, []) ->
    exodm_rpc_handler:update_session_counts().
%% start_phase(load_specs, _, []) ->
%%     %% exodm_rpc_spec:load_specs(),
%%     ok.
