-module(exodm_web_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0]).

start() ->
    application:start(exodm_web).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    exodm_web_sup:start_link().

stop(_State) ->
    ok.
