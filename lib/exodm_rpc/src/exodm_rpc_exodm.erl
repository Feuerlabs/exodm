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
	   {call, exodm, 'update-config-data',
	    [{'config-data', C},
	     {values, JSON}]}} = RPC, _Env) ->
    ?debug("update-config-data: config-data: ~p values:~p~n", [C, JSON]),
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
	    [{'dev-id', I}]}} = _RPC, _Env) ->
    ?debug("provision-device() device-id():~p ~n", [ I ]),
    exodm_db_device:new(exodm_db_session:get_aid(), I, []),
    {ok, result_code(ok)};


json_rpc_({request, _ReqEnv,
	   {call, exodm, 'add-config-data-members',
	    [{'config-data', {array, CfgList}},
	     {'dev-id', {array, DevList}}]}} = _RPC, _Env) ->
    ?debug("add-config-data-members~n", []),
    AID = exodm_db_session:get_aid(),
    F = fun(CfgData) ->
                exodm_db_config:add_config_data_members(AID, CfgData, DevList)
        end,
    try [ F(CfgData) || CfgData <- CfgList ] of
        _ ->  {ok, result_code(ok) }
    catch error: { unknown_config_data, CfgData} ->
            ?info("add-config-data-members(): Unknown config data: ~p\n",
                  [CfgData]),
	    {ok, result_code('object-not-found') };

          error: { unknown_device, Dev} ->
            ?info("add-config-data-members(): Unknown device-id ~p cannot be added to config data\n",
                  [Dev]),
	    {ok, result_code('device-not-found') }
    end;

json_rpc_(RPC, _ENV) ->
    ?error("~p:json_rpc_() Unknown RPC: ~p ~n", [ ?MODULE, RPC ]),
    {ok, result_code('validation-failed')}.

