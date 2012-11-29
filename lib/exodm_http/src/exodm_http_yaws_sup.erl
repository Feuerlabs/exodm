
-module(exodm_http_yaws_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([add_children/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% init(ChildSpecs) ->
%%     {ok, { {one_for_one, 5, 10}, ChildSpecs} }.
init([]) ->
    {ok, { {one_for_one, 3, 10}, [] }}.

add_children(ChildSpecs) ->
    [ok(supervisor:start_child(?MODULE, Ch)) || Ch <- ChildSpecs],
    ok.

ok({ok, _}) -> ok;
ok({ok, _, _}) -> ok;
ok(Other) ->
    error(Other).
