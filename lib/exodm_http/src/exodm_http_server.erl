-module(exodm_http_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([add_session/0, add_session/1, add_session/2]).
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include("log.hrl").

-record(conf, {id, gconf, docroot, sconf}).

start_link() ->
    case ets:info(?MODULE, name) of
	undefined -> ets:new(?MODULE, [set, public, named_table]);
	_ -> ok
    end,
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_session() ->
    {ok, Application} = application:get_application(),
    add_session(Application).

add_session(A) ->
    case application:get_env(A, yaws_sconf) of
	{ok, SC} ->
	    add_session(A, SC);
	_ ->
	    error(no_yaws_sconf)
    end.

add_session(A, SC) ->
    gen_server:call(?MODULE, {add_session, A, SC}).

init([]) ->
    case ets:lookup(?MODULE, conf) of
	[] ->
	    Sconfs = find_sconfs(),
	    ?debug("find_sconfs() -> ~p~n", [Sconfs]),
	    #conf{
		   id = Id,
		   gconf = GconfList,
		   docroot = Docroot,
		   sconf = Sconfs0
		 } = ConfR = yaws_conf(),
	    AllSconfs = default_sconfs(Sconfs0) ++ Sconfs,
	    {ok, SC, GC, ChildSpecs} =
		yaws_api:embedded_start_conf(Docroot, AllSconfs, GconfList, Id),
	    ?debug("Yaws:~n"
		   "  SC = ~p~n"
		   "  GC = ~p~n"
		   "  ChildSpecs = ~p~n", [SC, GC, ChildSpecs]),
	    set_exodm_options(Sconfs),
	    exodm_http_yaws_sup:add_children(ChildSpecs),
	    yaws_api:setconf(GC, SC),
	    ConfR1 = ConfR#conf{sconf = AllSconfs},
	    ets:insert(?MODULE, {conf, ConfR1}),
	    {ok, []};
	[_] ->
	    {ok, []}
    end.

handle_call({add_session, A, SC}, _, []) ->
    [{_, Conf}] = ets:lookup(?MODULE, conf),
    {_, _, Conf1} = Ret = add_session_(A, SC, Conf),
    ets:insert(?MODULE, {conf, Conf1}),
    Ret;
handle_call(_, _, St) ->
    {reply, error, St}.

handle_cast(_, St) ->
    {noreply, St}.

handle_info(_, St) ->
    {noreply, St}.

terminate(_, _) ->
    ok.

code_change(_, St, _) ->
    {ok, St}.

add_session_(A, SC0, #conf{id = Id,
			   gconf = GconfList,
			   docroot = Docroot,
			   sconf = Sconfs0} = Conf) ->
    SC = setup:expand_value(A, unwrap_sconf(SC0)),
    Sconfs = Sconfs0 ++ SC,
    {ok, SCy, GCy, _ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, Sconfs, GconfList, Id),
    case yaws_api:setconf(GCy, SCy) of
	{error, need_restart} ->
	    {stop, need_restart, Conf};
	_ ->
	    {reply, ok, Conf#conf{gconf = GCy, sconf = SCy}}
    end.

%% Assume lists are consistent, e.g. not [{sconf,SC1},[{id,...}|_]|...]
unwrap_sconf({sconf, SC}) -> [SC];
unwrap_sconf([{sconf,_}|_] = L) ->
    lists:map(fun({sconf,SC}) -> SC end, L);
unwrap_sconf([T|_] = SC) when is_tuple(T) ->
    [SC];
unwrap_sconf([[_|_]|_] = L) ->
    L.

default_sconfs(undefined) -> [];
default_sconfs(L) when is_list(L) -> L.

yaws_conf() ->
    Id = env(id, "embedded"),
    Docroot = env(docroot, filename:join(my_priv_dir(), "www")),
    LogDir = filename:join(setup:log_dir(), "yaws"),
    setup:verify_dir(LogDir),
    #conf{
	   id = Id,
	   gconf = [
		    {id, Id},
		    {log_dir, LogDir},
		    {ebin_dir, [Docroot]},
		    {include_dir, [Docroot]}
		   ],
	   docroot = Docroot
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

set_exodm_options(Sconfs) ->
    Opts = lists:flatmap(
	     fun(Conf) ->
		     case lists:keyfind(exodm_options, 1, Conf) of
			 {_, Opts} ->
			     {_, Port} = lists:keyfind(port, 1, Conf),
			     [{Port, Opts}];
			 false ->
			     []
		     end
	     end, Sconfs),
    OldOpts = case application:get_env(exodm_http, session_opts) of
		  {ok, Opts0} when is_list(Opts0) -> Opts0;
		  _ -> []
	      end,
    NewOpts = lists:foldl(fun({K,V}, Acc) -> orddict:store(K, V, Acc) end,
			  orddict:from_list(OldOpts), Opts),
    application:set_env(exodm_http, session_opts, NewOpts),
    ?debug("set_exodm_options(...) -> ~p~n", [NewOpts]),
    ok.

find_sconfs() ->
    lists:foldl(
      fun({_, SC}, Acc) ->
	      unwrap_sconf(SC) ++ Acc
      end, [], setup:find_env_vars(yaws_sconf)).
