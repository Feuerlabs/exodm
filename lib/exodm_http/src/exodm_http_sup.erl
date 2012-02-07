
-module(exodm_http_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-record(conf, {id, gconf, docroot, sconf}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    #conf{
      id = Id,
      gconf = GconfList,
      docroot = Docroot,
      sconf = SconfList
     } = yaws_conf(),
    {ok, _SCList, _GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    {ok, { {one_for_one, 5, 10}, ChildSpecs} }.

yaws_conf() ->
    Id = env(id, "embedded"),
    Docroot = env(docroot, filename:join(my_priv_dir(), "www")),
    Port = env(port, 8888),
    ServerName = env(servername, "exodm_http"),
    Listen = env(listen, {0,0,0,0}),
    AppMods = env(appmods, [{"/", exodm_http_yaws}]),
    #conf{
	   id = Id,
	   gconf = [{id, Id}],
	   docroot = Docroot,
	   sconf = [{port, Port},
		    {servername, ServerName},
		    {listen, Listen},
		    {docroot, Docroot},
		    {appmods, AppMods}]
	 }.

env(K, Def) ->
    case application:get_env(K) of
	{ok, V} when V =/= undefined ->
	    V;
	_ ->
	    Def
    end.

my_priv_dir() ->
    case application:get_application() of
	undefined ->
	    D = filename:join(
		  filename:dirname(
		    filename:dirname(code:which(?MODULE))),
		  "priv"),
	    case file:read_file_info(D) of
		{ok, _} ->
		    D;
		_ ->
		    {ok, Cwd} = file:get_cwd(),
		    Cwd
	    end;
	{ok, A} ->
	    code:priv_dir(A)
    end.
