
-module(exodm_web_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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

init([]) ->
    application:start(nprocreg),
    application:start(gproc),
    case application:start(kvdb) of
	{error,{already_started,kvdb}} ->
	    ok;
	ok ->
	    DbFile = filename:join(code:priv_dir(exodm_web), "exodm_web.db"),
	    kvdb_conf:open(DbFile)
    end,
    GettextServer = {gettext_server,{gettext_server,start_link,[{exodm_web,[]}]},
		     permanent,5000,worker,[gettext_server]},
    {ok, { {one_for_one, 5, 10}, 
	   [
	    GettextServer
	   ]} 
    }.

