-module(exodm_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start_phase/3]).

-include_lib("lager/include/log.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    exodm_sup:start_link().

stop(_State) ->
    ok.

start_phase(load_plugins, _, _) ->
    {ok, Plugins} = exodm_server:reload(),
    ?info("Plugins found and loaded: ~p~n", [Plugins]),
    ok.
