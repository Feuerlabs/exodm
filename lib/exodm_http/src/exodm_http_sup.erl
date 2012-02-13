
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
    #conf{
      id = Id,
      gconf = GconfList,
      docroot = Docroot,
      sconf = SconfList
     } = yaws_conf(),
    {ok, SC, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    io:fwrite("Yaws:~n"
	      "  SC = ~p~n"
	      "  GC = ~p~n", [SC, GC]),
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, ChildSpecs),
    yaws_api:setconf(GC, SC),
    {ok, Pid}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(ChildSpecs) ->
    {ok, { {one_for_one, 5, 10}, ChildSpecs} }.

yaws_conf() ->
    Id = env(id, "embedded"),
    Docroot = env(docroot, filename:join(my_priv_dir(), "www")),
    LogDir = filename:join(setup:log_dir(), "yaws"),
    setup:verify_dir(LogDir),
    Port = env(port, 8888),
    io:fwrite("Port = ~p~n", [Port]),
    ServerName = env(servername, "exodm_http"),
    Listen = env(listen, {0,0,0,0}),
    AppMods = env(appmods, [{"/ck3", exodm_ck3_appmod}]),
    Confs = find_sessions(),
    #conf{
	   id = Id,
	   gconf = [
		    {id, Id},
		    {log_dir, LogDir},
		    {ebin_dir, [Docroot]},
		    {include_dir, [Docroot]}
		   ],
	   docroot = Docroot,
	   %% sconf = [{port, Port},
	   %% 	    {servername, ServerName},
	   %% 	    {listen, Listen},
	   %% 	    {docroot, Docroot},
	   %% 	    {appmods, AppMods}]
	   sconf = Confs
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

find_sessions() ->
    dbg:tracer(),
    dbg:tpl(?MODULE, expand_env, x),
    dbg:tp(re, replace, x),
    dbg:p(all,[c]),
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
