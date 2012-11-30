-module(exodm_rpc_exodm).

-export([json_rpc/2]).
-include_lib("lager/include/log.hrl").

-define(USER_REPOSITORY, <<"user">>).
-define(SYSTEM_REPOSITORY, <<"system">>).

-define(EXO(M), (M==<<"exodm">> orelse M==<<"exosense">>)).

-define(catch_result(Expr),
	result_code(try Expr
		    catch
			error:'permission-denied' = E -> E;
			error:'validation-failed' = E -> E;
			error:'object-exists'     = E -> E;
			error:'device-not-found'  = E -> E;
			error:'object-not-found'  = E -> E;
			error:'object-not-empty'  = E -> E
		    end)).


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

result_code('object-not-empty') ->
    [{result, <<"object-not-empty">>}];

result_code('device-not-found') ->
    [{result, <<"device-not-found">>}].


json_rpc(RPC, Env) ->
    ?debug("~p:json_rpc(~p, ~p)~n", [?MODULE, RPC, Env]),
    json_rpc_(RPC, Env).


json_rpc_({call, M, <<"create-config-set">>,
	   [{'name', N, _},
	    {yang, Y, _} | Rest] = Attrs} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(create-config-set) name:~p yang:~p "
	   "Rest: ~p~n", [?MODULE, N, Y, Rest]),
    AID = exodm_db_session:get_aid(),
    try exodm_db_config:new_config_set(AID, kvl(Attrs)) of
	{ok, _} ->
	    ?debug("new_config_set(...) -> ok~n", []),
	    {ok, result_code(ok)}
    catch error:E ->
	    ?debug("RPC = ~p; ERROR = ~p~n",
		   [_RPC, {E, erlang:get_stacktrace()}]),
	    {error, E}
    end;


json_rpc_({call, M, <<"update-config-set">>,
	   [{'name', C, _}|Opts]} = RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(update-config-set) name:~p opts:~p~n",
           [ ?MODULE, C, Opts ]),

    AID = exodm_db_session:get_aid(),
    case exodm_db_config:update_config_set(AID, C, kvl(Opts)) of
	{ok, _} ->
	    ?debug("update_config_set(...) -> ok~n", []),
	    {ok, result_code(ok)};

        {error, Error} ->
	    ?debug("RPC = ~p; ERROR = ~p~n", [RPC, Error]),
	    {error, Error}
    end;

json_rpc_({call, M, <<"delete-config-set">>,
	   [{'name', C, _}]} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(delete-config-set) name:~p~n", [?MODULE, C]),
    AID = exodm_db_session:get_aid(),
    case exodm_db_config:delete_config_set(AID, C) of
	ok ->
	    ?debug("delete-config-set(...) -> ok~n", []),
	    {ok, result_code(ok)};
	{error, Error} ->
	    ?debug("delete-config-set(...) -> ERROR: ~p~n", [Error]),
	    {error, Error}
    end;

json_rpc_({call, M, <<"create-yang-module">>,
	   [{repository, R, _},
	    {name, N, _},
	    {'yang-module', Y, _}]} = _RPC, _Env) when R =:= ?USER_REPOSITORY,
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


json_rpc_({call, _, <<"provision-device">>,
	   [{'dev-id', I, _},
	    {'device-type', T, _} |
	    Opts]} = _RPC, _Env) ->
    ?debug("~p:json_rpc(provision-device) dev-id:~p "
	   "protocol:~p Optional:~p~n", [?MODULE, I, t, Opts]),

    exodm_db_device:new(exodm_db_session:get_aid(), I,
			[{'device-type', T}|kvl(Opts)]),
    {ok, result_code(ok)};

json_rpc_({call, _, <<"lookup-device">>,
	   [{'dev-id', I, _}]}, _Env) ->
    ?debug("~p:json_rpc(lookup-device) dev-id: ~p~n",
	   [?MODULE,I]),
    Res = exodm_db_device:lookup(exodm_db_session:get_aid(), I),
    case Res of
	[] ->
	    {ok, result_code('device-not-found')};
	[_|_] ->
	    {ok, result_code(ok) ++
		 [{devices, {array, [{struct, Res} || Res =/= []]}}]}
    end;

json_rpc_({call, _, <<"update-device">>,
	   [{'dev-id', I, _} | Opts]}, _Env) ->
    ?debug("~p:json_rpc(update-device) dev-id: ~p, Opts = ~p~n",
	   [?MODULE,I,Opts]),
    exodm_db_device:update(exodm_db_session:get_aid(), I, kvl(Opts)),
    {ok, result_code(ok)};


json_rpc_({call, _, <<"deprovision-devices">>,
	   [{'dev-id', DevIdList, _}]}, _Env) ->
    ?debug("~p:json_rpc(deprovision-devices) dev-id:~p~n",
           [ ?MODULE, DevIdList ]),
    exodm_db_device:delete_devices(exodm_db_session:get_aid(), DevIdList),
    {ok, result_code(ok)};

json_rpc_({call, M, <<"add-config-set-members">>,
	   [{'name', CfgDataList, _},
	    {'dev-id', DevIdList, _}]} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(add-config-set-members) config-sets:~p devices:~p~n",
           [ ?MODULE, CfgDataList, DevIdList ]),
    AID = exodm_db_session:get_aid(),

    try [ exodm_db_config:add_config_set_members(
	    AID, CfgD, DevIdList)
          || CfgD <- CfgDataList] of
        _Res -> {ok, result_code(ok)}
    catch error:E ->
	    ?debug("RPC = ~p; ERROR = ~p~n",
		   [_RPC, {E, erlang:get_stacktrace()}]),
	    {error, E}
    end;

json_rpc_({call, M, <<"add-device-group-members">>,
	   [{'device-groups', Groups, _},
	    {'dev-id', DIDs, _}]} = _RPC, _Env) when ?EXO(M) ->
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

json_rpc_({call, M, <<"remove-device-group-members">>,
	   [{'device-groups', Groups, _},
	    {'dev-id', DIDs, _}]} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(remove-device-group-members) dev-id:~p groups:~p~n",
           [ ?MODULE, DIDs, Groups ]),
    AID = exodm_db_session:get_aid(),

    try [ exodm_db_device:remove_groups(
	    AID, DID, Groups)
	  || DID <- DIDs] of
	_Res -> {ok, result_code(ok)}
    catch error:E ->
	    ?debug("RPC = ~p; ERROR = ~p~n",
		   [_RPC, {E, erlang:get_stacktrace()}]),
	    {error, E}
    end;

json_rpc_({call, M, <<"push-config-set">>,
	   [{'name', Cfg, _}]} = _RPC, Env0) when ?EXO(M) ->
    ?debug("~p:json_rpc(push-config-set) config-set:~p ~n", [?MODULE, Cfg]),
    TID = proplists:get_value(transaction_id, Env0),
    AID = exodm_db_session:get_aid(),
    User = exodm_db_session:get_user(),
    exodm_db:in_transaction(
      fun(Db) ->
	      case exodm_db_config:list_config_set_members(AID, Cfg) of
		  [_|_] = Devices ->
		      {ok, Yang} = exodm_db_config:get_yang_spec(AID, Cfg),
		      Module = filename:basename(Yang, ".yang"),
		      {ok, Ref} = exodm_db_config:cache_values(AID, Cfg),
		      RPC = {call, binary_to_atom(Module, latin1),
			     'push-config-set',
			     [{'name', Cfg},
			      {'reference', Ref}]},
		      Env = [{aid, AID}, {user, User},
			     {'transaction-id', TID},
			     {yang, <<"exodm.yang">>}|Env0],
		      %% Must map and queue in different loops
		      _ = [exodm_db_config:map_device_to_cached_values(
			     AID, Cfg, Ref, DID) || DID <- Devices],
		      _ = [exodm_rpc_handler:queue_message(
			     Db, AID, to_device,
			     [{'device-id', DID}|Env], RPC) || DID <- Devices],
		      exodm_db_config:switch_to_active(AID, Cfg),
		      {ok, result_code(ok)};
		  [] ->
		      %% Is this an error or ok?
		      {ok, result_code(ok)}
	      end
      end);

json_rpc_({call, M, <<"provision-device">>,
	   [{'device-id', DID, _}|Opts] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(provision-device) attributes:~p ~n",
	   [?MODULE, _Cfg]),
    AID = exodm_db_session:get_aid(),
    exodm_db:in_transaction(
      fun(_Db) ->
	      exodm_db_device:new(AID, DID, kvl(Opts))
      end),
    {ok, result_code(ok)};

json_rpc_({call, M, <<"list-devices">>,
	   [{n, N, _}, {previous, Prev, _}] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(delete-devices) config: ~p~n",
	   [?MODULE, _Cfg]),
    AID = exodm_db_session:get_aid(),
    Res =
	exodm_db:in_transaction(
	  fun(_) ->
		  exodm_db:list_next(exodm_db_device:table(AID), N, Prev,
				     fun(Key) ->
					     [DID|_] =
						 kvdb_conf:split_key(Key),
					     exodm_db_device:lookup(
					       AID, DID)
				     end)
	  end),
    ?debug("devices = ~p~n", [Res]),
    {ok, [{'devices',
	   {array, [ {struct, D} || D <- Res ]}
	  }]};


json_rpc_({call, M, <<"create-device-type">>,
	   [{'name', Name, _}|Opts] = _Cfg}, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(create-device-type, ~p) config: ~p~n",
	   [?MODULE, Name, _Cfg]),
    AID = exodm_db_session:get_aid(),
    ok = exodm_db_device_type:new(AID, Name, yang_json:remove_yang_info(Opts)),
    {ok, result_code(ok)};

json_rpc_({call, M, <<"update-device-type">>,
	   [{'name', Name, _}|Opts] = _Cfg}, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(update-device-type, ~p) config: ~p~n",
	   [?MODULE, Name, _Cfg]),
    AID = exodm_db_session:get_aid(),
    {ok, ?catch_result(
	    ok = exodm_db_device_type:update(
		   AID, Name, yang_json:remove_yang_info(Opts)))};

json_rpc_({call, M, <<"delete-device-type">>,
	   [{'name', Name, _}]}, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(delete-device-type) name: ~p~n", [?MODULE, Name]),
    AID = exodm_db_session:get_aid(),
    {ok, ?catch_result(
	    ok = exodm_db_device_type:delete(AID, Name))};

json_rpc_({call, M, <<"list-device-types">>,
	   [{'n', N, _},
	    {'previous', Prev, _}]} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(~p)~n", [_RPC]),
    AID = exodm_db_session:get_aid(),
    Res =
	exodm_db:in_transaction(
	  fun(_) ->
		  exodm_db:list_next(exodm_db_device_type:table(AID), N, Prev,
				     fun(Key) ->
					     [Name|_] =
						 kvdb_conf:split_key(Key),
					     exodm_db_device_type:lookup(
					       AID, Name)
				     end)
	  end),
    ?debug("device types = ~p~n", [Res]),
    {ok, [{'device-types',
	   {array, [ {struct, DT} || DT <- Res]}
	  }]};

json_rpc_({call, M, <<"create-device-group">>,
	   [{'name', GName, _},
	    {'notification-url', URL, _}] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(create-device-group) config: ~p~n", [?MODULE, _Cfg]),
    AID = exodm_db_session:get_aid(),
    {ok, GID} = exodm_db_group:new(AID, [{name, GName},
					 {url, URL}]),
    {ok, result_code(ok) ++ [{gid, to_uint32(exodm_db:group_id_value(GID))}]};

json_rpc_({call, M, <<"update-device-group">>,
	   [{'gid', GID, _}|Values] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(change-notification-url) config: ~p~n",
	   [?MODULE, _Cfg]),
    AID = exodm_db_session:get_aid(),
    Values2 = lists:map(fun({'notification-url',U, _}) -> {url,U};
			   ({K,V,_}) -> {K,V}
			end, Values),
    case exodm_db_group:update(AID, GID, Values2) of
	ok -> {ok, result_code(ok)};
	{error, not_found} ->
	    {ok, result_code('object-not-found')}
    end;

json_rpc_({call, M, <<"delete-device-group">>,
	   [{'gid', GID, _}] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(delete-device-group) config: ~p~n",
	   [?MODULE, _Cfg]),
    AID = exodm_db_session:get_aid(),
    case exodm_db_group:delete(AID, GID) of
	ok -> {ok, result_code(ok)};
	{error, not_found} ->
	    {ok, result_code('object-not-found')}
    end;

json_rpc_({call, M, <<"list-device-groups">>,
	   [{n, N, _}, {previous, Prev, _}] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(delete-device-group) config: ~p~n",
	   [?MODULE, _Cfg]),
    AID = exodm_db_session:get_aid(),
    Res =
	exodm_db:in_transaction(
	  fun(_) ->
		  exodm_db_group:list_groups(AID, N, Prev)
	  end),
    ?debug("groups = ~p~n", [Res]),
    {ok, [{'device-groups',
	   {array, [
		    {struct,
		     [{gid, to_uint32(exodm_db:group_id_value(G))},
		      {name, Nm},
		      {'notification-url', U}]} ||
		       [{id,G},{name,Nm},{url,U}|_] <- Res]}}]};

json_rpc_({call, M, <<"list-device-type-members">>,
	   [{'name', T, _}, {'n', N, _}, {'previous', Prev, _}] = Params},
	  _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(list-device-type-members) args = ~p~n",
	   [?MODULE,Params]),
    AID = exodm_db_session:get_aid(),
    Res =
	exodm_db:in_transaction(
	  fun(_) ->
		  FullNext = kvdb_conf:join_key(
			       [exodm_db:account_id_key(AID),
				T, <<"members">>, Prev]),
		  exodm_db:list_next(exodm_db_device_type:table(AID),
				     N, FullNext,
				     fun(Key) ->
					     lists:last(
					       kvdb_conf:split_key(Key))
				     end)
	  end),
    ?debug("device type members (~p) = ~p~n", [T, Res]),
    {ok, [{'device-type-members', {array, Res}}]};



json_rpc_({call, M, <<"list-config-sets">>,
	   [{n, N, _}, {previous, Prev, _}] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(list-config-sets) args: ~p~n",
	   [?MODULE, _Cfg]),
    AID = exodm_db_session:get_aid(),
    Res =
	exodm_db:in_transaction(
	  fun(_) ->
		  %% FullNext = kvdb_conf:join_key(exodm_db:account_id_key(AID),
		  %% 				Prev),
		  exodm_db:list_next(exodm_db_config:table(AID), N, Prev,
				     fun(Key) ->
					     [CfgSet|_] =
						 kvdb_conf:split_key(Key),
					     exodm_db_config:read_config_set(
					       AID, CfgSet)
				     end)
	  end),
    ?debug("config sets = ~p~n", [Res]),
    {ok, [{'config-sets',
	   {array, [
		    {struct,
		     [{name, Nm},
		      {yang, Y},
		      {'notification-url', U}]} ||
		       [{name,Nm},{yang,Y},{'notification-url',U}|_] <- Res]}}]};

json_rpc_({call, M, <<"list-config-set-members">>,
	   [{'name', C, _}, {'n', N, _}, {'previous', Prev, _}] = Params},
	  _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(list-config-set-members) args = ~p~n", [?MODULE,Params]),
    AID = exodm_db_session:get_aid(),
    Res =
	exodm_db:in_transaction(
	  fun(_) ->
		  FullNext = kvdb_conf:join_key([exodm_db:account_id_key(AID),
						 C, <<"members">>, Prev]),
		  exodm_db:list_next(exodm_db_config:table(AID), N, FullNext,
				     fun(Key) ->
					     lists:last(kvdb_conf:split_key(Key))
				     end)
	  end),
    ?debug("config set members = ~p~n", [Res]),
    {ok, [{'config-set-members', {array, Res}}]};

json_rpc_({call, M, <<"list-device-group-members">>,
	   [{'gid', G, _}, {'n', N, _}, {'previous', Prev, _}] = Params},
	  _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(list-device-group-members) args = ~p~n", [?MODULE,Params]),
    AID = exodm_db_session:get_aid(),
    Res =
	exodm_db:in_transaction(
	  fun(_) ->
		  exodm_db_group:list_devices(AID,G,N,Prev)
		  %% FullNext = kvdb_conf:join_key([exodm_db:account_id_key(AID),
		  %% 				 C, <<"members">>, Prev]),
		  %% exodm_db:list_next(exodm_db_config:table(AID), N, FullNext,
		  %% 		     fun(Key) ->
		  %% 			     lists:last(kvdb_conf:split_key(Key))
		  %% 		     end)
	  end),
    ?debug("device group members = ~p~n", [Res]),
    {ok, [{'device-group-members', {array, Res}}]};

json_rpc_(RPC, _ENV) ->
    ?info("~p:json_rpc_() Unknown RPC: ~p ~n", [ ?MODULE, RPC ]),
    {ok, result_code('validation-failed')}.


to_uint32(<<I:32>>) -> I;
to_uint32(I) when is_integer(I) -> I.

kvl([{K,V}  |Opts]) -> [{K,V}|kvl(Opts)];
kvl([{K,V,_}|Opts]) -> [{K,V}|kvl(Opts)];
kvl([]) -> [].

