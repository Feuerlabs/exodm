-module(exodm_rpc_exodm).

-export([json_rpc/2]).
-include_lib("lager/include/log.hrl").
-define(USER_REPOSITORY, "user").
-define(SYSTEM_REPOSITORY, "system").

result_code(ok) ->
    [{ result, <<"ok">> }];

result_code('permission-denied') ->
    [{ result, <<"permission-denied">> }];

result_code('validation-failed') ->
    [{ result, <<"validation-failed">> }];

result_code('object-exists') ->
    [{ result, <<"object-exists">> }];

result_code('object-not-found') ->
    [{ result, <<"object-not-found">> }].


json_rpc(RPC, Env) ->
    ?debug("~p:json_rpc(~p, ~p)~n", [?MODULE, RPC, Env]),
    json_rpc_(RPC, Env).

json_rpc_({request, _Env,
	   {call, exodm, 'create-config-data', [{name, N},
						{yang, Y},
						{protocol, P},
						{values, JSON}]}} = _RPC, _Env) ->
    AID = exodm_db_session:get_aid(),
    try exodm_db_config:new_config_data(AID, N, Y, P, JSON) of
	{ok, _} ->
	    ?debug("new_config_data(...) -> ok~n", []),
	    {ok, result_code(ok)}
    catch error:E ->
	    ?debug("RPC = ~p; ERROR = ~p~n",
		   [_RPC, {E, erlang:get_stacktrace()}]),
	    {error, E}
    end;

json_rpc_({request, _Env,
	   {call, exodm, 'create-yang-module', [{name, N},
						{repository, ?USER_REPOSITORY},
						{'yang-module', Y}]}} = _RPC, _Env) ->
    try exodm_db_yang:write(N, Y) of
	{ok, _} ->
	    ?debug("create-yang-module(...) -> ok~n", []),
	    {ok, result_code(ok)}

    catch error:E ->
	    ?debug("RPC = ~p; ERROR = ~p~n",
		   [_RPC, {E, erlang:get_stacktrace()}]),
	    {error, E}
    end.

