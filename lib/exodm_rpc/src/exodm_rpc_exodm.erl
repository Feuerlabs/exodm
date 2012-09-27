-module(exodm_rpc_exodm).

-export([json_rpc/2]).
-include_lib("lager/include/log.hrl").

-define(USER_REPOSITORY, <<"user">>).
-define(SYSTEM_REPOSITORY, <<"system">>).

-define(EXO(M), (M==exodm orelse M==exosense)).

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
	   {call, M, 'create-config-set',
	    [{'name', N},
	     {yang, Y},
	     {'notification-url', URL},
	     {values, JSON}] = Attrs}} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(create-config-set) name:~p yang:~p "
	   "URL: ~p values:~p~n", [?MODULE, N, Y, URL, JSON]),
    AID = exodm_db_session:get_aid(),
    try exodm_db_config:new_config_set(AID, Attrs) of
	{ok, _} ->
	    ?debug("new_config_set(...) -> ok~n", []),
	    {ok, result_code(ok)}
    catch error:E ->
	    ?debug("RPC = ~p; ERROR = ~p~n",
		   [_RPC, {E, erlang:get_stacktrace()}]),
	    {error, E}
    end;


json_rpc_({request, _ReqEnv,
	   {call, M, 'update-config-set',
	    [{'name', C}|Opts]}} = RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(update-config-set) name:~p opts:~p~n",
           [ ?MODULE, C, Opts ]),

    AID = exodm_db_session:get_aid(),
    case exodm_db_config:update_config_set(AID, C, Opts) of
	{ok, _} ->
	    ?debug("update_config_set(...) -> ok~n", []),
	    {ok, result_code(ok)};

        {error, Error} ->
	    ?debug("RPC = ~p; ERROR = ~p~n", [RPC, Error]),
	    {error, Error}
    end;


json_rpc_({request, _ReqEnv,
	   {call, M, 'create-yang-module',
	    [{repository, R},
	     {name, N},
	     {'yang-module', Y}]}} = _RPC, _Env) when R =:= ?USER_REPOSITORY,
						      ?EXO(M) ->
    ?debug("~p:json_rpc(create-yang-module) repository:~p name:~p~n",
	   [ ?MODULE, R, N ]),
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
	   {call, M, 'provision-device',
	    [{'dev-id', I},
	     {'protocol', P} |
	     Opts]}} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(provision-device) dev-id:~p "
	   "protocol:~p Optional:~p~n", [?MODULE, I, P, Opts]),

    exodm_db_device:new(exodm_db_session:get_aid(), I,
			[{'protocol', P}|Opts]),
    {ok, result_code(ok)};




json_rpc_({request, _ReqEnv,
	   {call, M, 'add-config-set-members',
	    [{'name', CfgDataList},
             {'dev-id', DevIdList}]}} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(add-config-set-members) dev-id:~p config-set:~p~n",
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

json_rpc_({request, _ReqEnv,
	   {call, M, 'add-device-group-members',
	    [{'device-groups', Groups},
	     {'dev-id', DIDs}]}} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(add-device-group-members) dev-id:~p groups:~p~n",
           [ ?MODULE, DIDs, Groups ]),
    AID = exodm_db_session:get_aid(),

    try [ exodm_db_device:add_groups(
	    AID, DID, Groups)
	  || DID <- DIDs] of
	_Res -> {ok, result_code(ok)}
    catch error:E ->
	    ?debug("RPC = ~p; ERROR = ~p~n",
		   [_RPC, {E, erlang:get_stacktrace()}]),
	    {error, E}
    end;

json_rpc_({request, ReqEnv,
	   {call, M, 'push-config-set',
	    [{'name', Cfg}]}} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(push-config-set) config-set:~p ~n", [?MODULE, Cfg]),
    TID = proplists:get_value(transaction_id, ReqEnv),
    AID = exodm_db_session:get_aid(),
    User = exodm_db_session:get_user(),
    exodm_db:in_transaction(
      fun(Db) ->
	      case exodm_db_config:list_config_data_members(AID, Cfg) of
		  [_|_] = Devices ->
		      {ok, Yang} = exodm_db_config:get_yang_spec(AID, Cfg),
		      Module = filename:basename(Yang, ".yang"),
		      {ok, Ref} = exodm_db_config:cache_values(AID, Cfg),
		      RPC = {request, [{'transaction-id', TID}],
			     {call, binary_to_atom(Module, [latin1]),
			      'push-config-set',
			      [{'name', Cfg},
			       {'reference', Ref}]}},
		      Env = [{aid, AID}, {user, User}],
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

json_rpc_({request, _ReqEnv,
	   {call, M, 'provision-device',
	    [{'device-id', DID},
	     {'server-key', SKey},
	     {'device-key', DKey}] = _Cfg}} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(provision-device) attributes:~p ~n",
	   [?MODULE, _Cfg]),
    AID = exodm_db_session:get_aid(),
    exodm_db:in_transaction(
      fun(_Db) ->
	      exodm_db_device:new(AID, DID, [{'__ck', DKey},
					     {'__sk', SKey}])
      end),
    {ok, result_code(ok)};

json_rpc_({request, _ReqEnv,
	   {call, M, 'create-device-group',
	    [{'name', GName},
	     {'notification-url', URL}] = _Cfg}} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(create-device-group) config: ~p~n", [?MODULE, _Cfg]),
    AID = exodm_db_session:get_aid(),
    {ok, GID} = exodm_db_group:new(AID, [{name, GName},
					 {url, URL}]),
    {ok, result_code(ok) ++ [{gid, to_uint32(exodm_db:group_id_value(GID))}]};

json_rpc_({request, _ReqEnv,
	   {call, M, 'update-device-group',
	    [{'gid', GID}|Values] = _Cfg}} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(change-notification-url) config: ~p~n",
	   [?MODULE, _Cfg]),
    AID = exodm_db_session:get_aid(),
    Values2 = lists:map(fun({'notification-url',U}) -> {url,U};
			   (X) -> X
			end, Values),
    case exodm_db_group:update(AID, GID, Values2) of
	ok -> {ok, result_code(ok)};
	{error, not_found} ->
	    {ok, result_code('object-not-found')}
    end;

json_rpc_({request, _ReqEnv,
	   {call, M, 'delete-device-group',
	    [{'gid', GID}] = _Cfg}} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(delete-device-group) config: ~p~n",
	   [?MODULE, _Cfg]),
    AID = exodm_db_session:get_aid(),
    case exodm_db_group:delete(AID, GID) of
	ok -> {ok, result_code(ok)};
	{error, not_found} ->
	    {ok, result_code('object-not-found')}
    end;

json_rpc_({request, _ReqEnv,
	   {call, M, 'list-device-groups',
	    [{n, N}, {previous, Prev}] = _Cfg}} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(delete-device-group) config: ~p~n",
	   [?MODULE, _Cfg]),
    AID = exodm_db_session:get_aid(),
    Res =
	exodm_db:in_transaction(
	  fun(_) ->
		  FullNext = kvdb_conf:join_key([exodm_db:account_id_key(AID),
						 <<"groups">>,
						 exodm_db:group_id_key(Prev)]),
		  exodm_db:list_next(<<"acct">>, N, FullNext,
				     fun(Key) ->
					     [_, _, Grp] =
						 kvdb_conf:split_key(Key),
					     exodm_db_group:lookup(AID, Grp)
				     end)
	  end),
    ?debug("groups = ~p~n", [Res]),
    {ok, [{groups,
	   {array, [
		    {struct,
		     [{gid, to_uint32(exodm_db:group_id_value(G))},
		      {name, Nm},
		      {'notification-url', U}]} ||
		       [{id,G},{name,Nm},{url,U}|_] <- Res]}}]};

json_rpc_(RPC, _ENV) ->
    ?info("~p:json_rpc_() Unknown RPC: ~p ~n", [ ?MODULE, RPC ]),
    {ok, result_code('validation-failed')}.


to_uint32(<<I:32>>) -> I;
to_uint32(I) when is_integer(I) -> I.
