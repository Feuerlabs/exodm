-module(exodm_rpc_exodm).

-export([json_rpc/2]).
-include_lib("lager/include/log.hrl").

-define(USER_REPOSITORY, <<"user">>).
-define(SYSTEM_REPOSITORY, <<"system">>).

result_code(ok) ->
    [{result, <<"ok">>}];

result_code('permission-denied') ->
    [{result, <<"permission-denied">>}];

result_code('validation-failed') ->
    [{result, <<"validation-failed">>}];

result_code('object-exists') ->
    [{result, <<"object-exists">>}];

result_code('object-not-found') ->
    [{result, <<"object-not-found">>}].


json_rpc(RPC, Env) ->
    ?debug("~p:json_rpc(~p, ~p)~n", [?MODULE, RPC, Env]),
    json_rpc_(RPC, Env).

json_rpc_({request, _ReqEnv,
	   {call, exodm, 'create-config-data',
	    [{name, N},
	     {yang, Y},
	     {protocol, P},
	     {values, JSON}]}} = _RPC, _Env) ->
    ?debug("'create-config-data'~n", []),
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

json_rpc_({request, _ReqEnv,
	   {call, exodm, 'set-config-value',
	    [{'cfg-data', C},
	     {values, JSON}]}} = RPC, _Env) ->
    ?debug("set-config-data: cfg-data: ~p values:~p~n", [C, JSON]),
    AID = exodm_db_session:get_aid(),
    case exodm_db_config:update_config_data(AID, C, JSON) of
	{ok, _} ->
	    ?debug("set_config_data(...) -> ok~n", []),
	    {ok, result_code(ok)};

        {error, Error} ->
	    ?debug("RPC = ~p; ERROR = ~p~n", [RPC, Error]),
	    {error, Error}
    end;


json_rpc_({request, _ReqEnv,
	   {call, exodm, 'create-yang-module',
	    [{repository, R},
	     {name, N},
	     {'yang-module', Y}]}} = _RPC, _Env) when R =:= ?USER_REPOSITORY->
    ?debug("'create-yang-module'~n", []),

    Res = exodm_db_yang:write(N, Y),
    io:format("YANG WRITE: ~p\n", [Res]),
    {ok, result_code(ok)};


json_rpc_({request, _ReqEnv,
	   {call, exodm, 'provision-device',
	    [{'new-device-id', I}]}} = _RPC, _Env) ->
    ?debug("provision-device() device-id():~p ~n", [ I ]),
    exodm_db_device:new(exodm_db_session:get_aid(), I, []),
    {ok, result_code(ok)};

json_rpc_({request, _ReqEnv,
	   {call, exodm, 'add-config-data-members',
	    [{repository, R},
	     {name, N},
	     {'yang-module', Y}]}} = _RPC, _Env) ->
    ?debug("create-yang-module~n", []),
    io:format("YANG WRONG REPO: Name:~p Repo:~p Yang:~p\n", [N,R,Y]),
    {ok, result_code('permission-denied') };

json_rpc_(RPC, _ENV) ->


    ?error("~p:json_rpc_() Unknown RPC: ~p ~n", [ ?MODULE, RPC ]),
    {ok, result_code('validation-failed')}.

