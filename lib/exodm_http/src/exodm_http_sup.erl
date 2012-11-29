
-module(exodm_http_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% -record(conf, {id, gconf, docroot, sconf}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    %% Sconfs = find_sconfs(),
    %% #conf{
    %%   id = Id,
    %%   gconf = GconfList,
    %%   docroot = Docroot,
    %%   sconf = Sconfs0
    %%  } = yaws_conf(),
    %% AllSconfs = default_sconfs(Sconfs0) ++ Sconfs,
    %% {ok, SC, GC, ChildSpecs} =
    %%     yaws_api:embedded_start_conf(Docroot, AllSconfs, GconfList, Id),
    %% io:fwrite("Yaws:~n"
    %% 	      "  SC = ~p~n"
    %% 	      "  GC = ~p~n", [SC, GC]),
    %% set_exodm_options(Sconfs),
    %% {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, ChildSpecs),
    %% yaws_api:setconf(GC, SC),
    %% {ok, Pid}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% init(ChildSpecs) ->
%%     {ok, { {one_for_one, 5, 10}, ChildSpecs} }.
init([]) ->
    {ok, { {one_for_all, 3, 10},
	   [{yaws_sup, {exodm_http_yaws_sup, start_link, []},
	     permanent, infinity, supervisor, [exodm_http_yaws_sup]},
	    {http_server, {exodm_http_server, start_link, []},
	     permanent, 2000, worker, [exodm_http_server]}] }}.

