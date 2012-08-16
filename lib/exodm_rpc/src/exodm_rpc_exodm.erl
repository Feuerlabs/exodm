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
    [{result, <<"object-not-found">>}];

result_code('device-not-found') ->
    [{result, <<"device-not-found">>}].


json_rpc(RPC, Env) ->
    ?debug("~p:json_rpc(~p, ~p)~n", [?MODULE, RPC, Env]),
    json_rpc_(RPC, Env).


json_rpc_({request, _ReqEnv,
	   {call, exodm, 'create-config-data',
	    [{'config-data', N},
	     {yang, Y},
	     {protocol, P},
	     {values, JSON}]}} = _RPC, _Env) ->
    ?debug("~p:json_rpc(create-config-data) config-data:~p yang:~p protocol:~p values:~p~n",
           [ ?MODULE, N, Y, P, JSON ]),
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
	   {call, exodm, 'update-config-data',
	    [{'config-data', C},
	     {values, JSON}]}} = RPC, _Env) ->
    ?debug("~p:json_rpc(update-config-data) config-data:~p values:~p~n",
           [ ?MODULE, C, JSON ]),

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
    ?debug("~p:json_rpc(create-yang-module) repository:~p name:~p~n", [ ?MODULE, R, N ]),
    _Res = exodm_db_yang:write(N, Y),
    {ok, result_code(ok)};


json_rpc_({request, _ReqEnv,
	   {call, exodm, 'provision-device',
	    [{'dev-id', I},
             {'server-key', SK},
             {'device-key', DK}]}} = _RPC, _Env) ->
    ?debug("~p:json_rpc(provision-device) device-id:~p server-key:~p device-key:~p~n",
           [ ?MODULE, I, SK, DK ]),

    exodm_db_device:new(exodm_db_session:get_aid(), I, [{'__ck', <<DK:64/little>>},
                                                        {'__sk', <<SK:64/little>>}]),
    {ok, result_code(ok)};


json_rpc_({request, _ReqEnv,
	   {call, exodm, 'push-config-data',
	    [{'config-data', Cfg}]}} = _RPC, _Env) ->
    ?debug("~p:json_rpc(push-config-data) config-data:~p ~n", [ ?MODULE, Cfg ]),
    _AID = exodm_db_session:get_aid(),
    {ok, result_code(ok)};

json_rpc_(RPC, _ENV) ->
    ?info("~p:json_rpc_() Unknown RPC: ~p ~n", [ ?MODULE, RPC ]),
    {ok, result_code('validation-failed')}.

