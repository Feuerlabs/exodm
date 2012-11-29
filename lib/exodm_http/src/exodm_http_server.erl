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
	    #conf{
		   id = Id,
		   gconf = GconfList,
		   docroot = Docroot,
		   sconf = Sconfs0
		 } = ConfR = yaws_conf(),
	    AllSconfs = default_sconfs(Sconfs0) ++ Sconfs,
	    {ok, SC, GC, ChildSpecs} =
		yaws_api:embedded_start_conf(Docroot, AllSconfs, GconfList, Id),
	    io:fwrite("Yaws:~n"
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
    SC = expand_sconf(SC0, A),
    Sconfs = Sconfs0 ++ [SC],
    {ok, SCy, GCy, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, Sconfs, GconfList, Id),
    case yaws_api:setconf(GCy, SCy) of
	{error, need_restart} ->
	    {stop, need_resart, Conf};
	_ ->
	    {reply, ok, Conf#conf{gconf = GCy, sconf = SCy}}
    end.



default_sconfs(undefined) -> [];
default_sconfs(L) when is_list(L) -> L.

yaws_conf() ->
    Id = env(id, "embedded"),
    Docroot = env(docroot, filename:join(my_priv_dir(), "www")),
    LogDir = filename:join(setup:log_dir(), "yaws"),
    setup:verify_dir(LogDir),
    Port = env(port, 8888),
    io:fwrite("Port = ~p~n", [Port]),
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
    io:fwrite("set_exodm_options(...) -> ~p~n", [NewOpts]),
    ok.

find_sconfs() ->
    %% TODO: Use setup:find_env_vars/1 instead, as it does exactly this, and then some.
    As = application:loaded_applications(),
    lists:reverse(
      lists:foldl(
	fun({A,_,_}, Acc) ->
		case application:get_env(A, yaws_sconf) of
		    {ok, SC} when is_list(SC) ->
			Conf = expand_sconf(SC, A),
			io:fwrite("Conf (~p): ~p~n", [A, Conf]),
			[Conf | Acc];
		    _ ->
			Acc
		end
	end, [], As)).

expand_sconf(L, A) when is_list(L) ->
    case is_string(L) of
	true ->
	    Envs = [{K, env_value(K, A)} || K <- ["PRIV_DIR", "LIB_DIR"]],
	    expand_env(Envs, L);
	false ->
	    [expand_sconf(X, A) || X <- L]
    end;
expand_sconf(T, A) when is_tuple(T) ->
    list_to_tuple([expand_sconf(X, A) || X <- tuple_to_list(T)]);
expand_sconf(X, _) ->
    X.

is_string(L) ->
    lists:all(fun(X) when 0 =< X, X =< 255 -> true;
		 (_) -> false
	      end, L).

expand_env(Vs, S) ->
    lists:foldl(fun({K, Val}, Sx) ->
			re:replace(Sx, [$\\, $$ | K], Val, [{return,list}])
		end, S, Vs).

env_value("PRIV_DIR", A) -> code:priv_dir(A);
env_value("LIB_DIR" , A) -> code:lib_dir(A).
