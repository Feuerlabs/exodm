%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Ulf Wiger <ulf@feuerlabs.com>
%%%
-module(exodm_server).
-behaviour(gen_server).

-export([start_link/0]).

-export([reload/0]).
-export([transform_plugins/0]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2]).

-include_lib("lager/include/log.hrl").

-record(st, {plugins = []}).

start_link() ->
    exodm_rpc_protocol:create_table(),
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

reload() ->
    gen_server:call(?MODULE, reload).

init([]) ->
    {ok, #st{}}.

handle_call(reload, _, St) ->
    {Result, St1} = reload_(St),
    {reply, Result, St1};
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


reload_(#st{} = St) ->
    {ok, AppNames} = load_plugins(),
    {{ok, AppNames}, St#st{plugins = AppNames}}.

transform_plugins() ->
    AllPlugins = find_plugins(),
    load_apps(AllPlugins),
    setup:run_hooks(convert_plugin, [A || {A,_} <- AllPlugins]).

find_plugins() ->
    ?debug("ERL_SETUP_LIBS = ~p~n", [os:getenv("ERL_SETUP_LIBS")]),
    LibDirs = setup:lib_dirs("ERL_SETUP_LIBS"),
    ?debug("LibDirs = ~p~n", [LibDirs]),
    AppNames = app_names(LibDirs),
    ?debug("AppNames = ~p~n", [AppNames]),
    [{A,setup:pick_vsn(A, setup:find_app(A, LibDirs), latest)} ||
	A <- AppNames].


load_plugins() ->
    AllPlugins = find_plugins(),
    load_apps(AllPlugins),
    register_protocols(AllPlugins),
    start_apps(AllPlugins),
    {ok, [A || {A,_} <- AllPlugins]}.

load_apps(AllPlugins) ->
    lists:foreach(
      fun({A,{V,D}}) ->
	      true = setup:patch_app(A, V, [D]),
	      ReloadRes = setup:reload_app(A, V, [D]),
	      ?debug("reload_app(~p, ~p, ~p) -> ~p~n", [A,V,[D], ReloadRes])
      end, AllPlugins).

register_protocols(AllPlugins) ->
    lists:foreach(
      fun({A, _}) ->
	      RegisterRes = register_protocol(A),
	      ?debug("register_protocol(~p) -> ~p~n", [A, RegisterRes])
      end, AllPlugins).

start_apps(AllPlugins) ->
    lists:foreach(
      fun({A, _}) ->
	      maybe_start(A)
      end, AllPlugins).


app_names(Dirs) ->
    lists:usort(
      lists:flatmap(
	fun(D) ->
		case lists:reverse(filename:split(D)) of
		    ["ebin", A | _] ->
			[AName|_] = re:split(A,"-",[{return,list}]),
			[list_to_atom(AName)];
		    _ ->
			[]
		end
	end, Dirs)).

register_protocol(App) ->
    case application:get_env(App, exodm_protocol) of
	{ok, {Module, Mode}} when is_atom(Module),
				  Mode==queued;
				  Mode==direct ->
	    exodm_rpc_protocol:register_protocol(App, Module, Mode),
	    ok;
	{ok, {Protocol, Module, Mode}} when is_binary(Protocol),
					    is_atom(Module),
					    Mode == queued;
					    Mode == direct ->
	    exodm_rpc_protocol:register_protocol(Protocol, App, Module, Mode),
	    ok;
	Other ->
	    {error, {unknown_protocol, Other}}
    end.

maybe_start(App) ->
    case application:get_key(App, mod) of
	{ok, {_, _}} ->
	    %% Can be started
	    application:start(App);
	_ ->
	    ok
    end.
