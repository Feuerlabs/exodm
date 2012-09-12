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
    Res = exodm_db_yang:write(N, Y),
    io:format("YANG_RES: ~p\n", [ Res]),
    case Res of
        ok ->
            {ok, result_code(ok)};

        {error, {Line, Fmt, Arg } } ->
            ?info("~p:json_rpc(create-yang-module): Error: Line: ~p: ~p\n",
                 [?MODULE, Line, io_lib:fwrite(Fmt, Arg) ]),
            {ok, result_code('validation-failed')};

        Err ->
            ?info("~p:json_rpc(create-yang-module): Error: ~p\n",
                 [?MODULE, Err ]),
            {ok, result_code('validation-failed')}
    end;


json_rpc_({request, _ReqEnv,
	   {call, exodm, 'provision-device',
	    [{'dev-id', I},
             {'server-key', SK},
             {'device-key', DK}]}} = _RPC, _Env) ->
    ?debug("~p:json_rpc(provision-device) dev-id:~p server-key:~p device-key:~p~n",
           [ ?MODULE, I, SK, DK ]),

    exodm_db_device:new(exodm_db_session:get_aid(), I, [{'__ck', <<DK:64/little>>},
                                                        {'__sk', <<SK:64/little>>}]),
    {ok, result_code(ok)};




json_rpc_({request, _ReqEnv,
	   {call, exodm, 'add-config-data-members',
	    [{'config-data', {array, CfgDataList}},
             {'dev-id', {array, DevIdList}}]}} = _RPC, _Env) ->
    ?debug("~p:json_rpc(add-config-data-members) dev-id:~p config-data:~p~n",
           [ ?MODULE, CfgDataList, DevIdList ]),
    AID = exodm_db_session:get_aid(),

    try [ exodm_db_config:add_config_data_members(
	    AID, CfgD, DevIdList)
          || CfgD <- CfgDataList] of
        _Res -> {ok, result_code(ok)}
    catch error:E ->
	    ?debug("RPC = ~p; ERROR = ~p~n",
		   [_RPC, {E, erlang:get_stacktrace()}]),
	    {error, E}
    end;

json_rpc_({request, ReqEnv,
	   {call, exodm, 'push-config-data',
	    [{'config-data', Cfg}]}} = _RPC, _Env) ->
    ?debug("~p:json_rpc(push-config-data) config-data:~p ~n", [?MODULE, Cfg]),
    TID = proplists:get_value(transaction_id, ReqEnv),
    AID = exodm_db_session:get_aid(),
    User = exodm_db_session:get_user(),
    exodm_db:in_transaction(
      fun(Db) ->
	      case exodm_db_config:list_config_data_members(AID, Cfg) of
		  [_|_] = Devices ->
		      {ok, Yang} = exodm_db_config:get_yang_spec(AID, Cfg),
		      {ok, Proto} = exodm_db_config:get_protocol(AID, Cfg),
		      Module = filename:basename(Yang, ".yang"),
		      {ok, Ref} = exodm_db_config:cache_values(AID, Cfg),
		      RPC = {request, [{'transaction-id', TID}],
			     {call, binary_to_atom(Module, [latin1]),
			      'push-config-data',
			      [{'config-data', Cfg},
			       {'reference', Ref}]}},
		      Env = [{aid, AID}, {user, User}, {protocol, Proto}],
		      lists:foreach(
			fun(DID) ->
				exodm_db_config:map_device_to_cached_values(
				  AID, Cfg, Ref, DID),
				exodm_rpc_handler:queue_message(
				  Db, AID, to_device,
				  [{'device-id', DID}|Env], RPC)
			end, Devices),
		      {ok, result_code(ok)};
		  [] ->
		      %% Is this an error or ok?
		      {ok, result_code(ok)}
	      end
      end);

json_rpc_({request, ReqEnv,
	  {call, exodm, 'provision-device',
	  [{'device-id', DID},
	   {'server-key', SKey},
	   {'device-key', DKey}] = _Cfg}} = _RPC, _Env) ->
    ?debug("~p:json_rpc(provision-device) config-data:~p ~n",
	   [?MODULE, _Cfg]),
    AID = exodm_db_session:get_aid(),
    exodm_db_device:new(AID, DID, [{'__ck', DKey},
				   {'__sk', SKey}]),
    {ok, result_code(ok)};

json_rpc_(RPC, _ENV) ->
    ?info("~p:json_rpc_() Unknown RPC: ~p ~n", [ ?MODULE, RPC ]),
    {ok, result_code('validation-failed')}.