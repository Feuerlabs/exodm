-module(exodm_rpc_exodm).

-export([json_rpc/2,
	 exodm_admin/1]).
-include_lib("lager/include/log.hrl").

-define(USER_REPOSITORY, <<"user">>).
-define(SYSTEM_REPOSITORY, <<"system">>).

-define(EXO(M), (M==<<"exodm">> orelse M==<<"exosense">>)).
-define(ADMIN(M), (M==<<"exodm_admin">>)).

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


%%--------------------------------------------------------------------
%% @doc
%% Checks if User is the system admin.
%%
%% @end
%%--------------------------------------------------------------------
-spec exodm_admin(UID::binary()) -> boolean().

exodm_admin(User) -> ?ADMIN(User).
    

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
	   [{'name', _N, _},
	    {yang, _Y, _} | _Rest] = Attrs} = _RPC, _Env) when ?EXO(M) ->
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
    %% FIXME!! we should be sure that the user can not create 
    %% exodm.yang | exosense.yang | exodm_admin.yang 
    %% (this will disable) the user
    Res = exodm_db_yang:write(N, Y),
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

json_rpc_({call, M, <<"create-yang-module">>,
	   [{repository, R, _},
	    {name, N, _},
	    {'yang-module', Y, _}]} = _RPC, Env) when R =:= ?SYSTEM_REPOSITORY,
						      ?EXO(M) ->
    case has_root_access(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res =
		case exodm_db_yang:write_system(N, Y) of
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
		end,
	    exodm_db_session:unset_trusted_proc(),
	    Res;
	false ->
	    {ok,result_code('permission-denied')}
    end;

json_rpc_({call, M, <<"delete-yang-module">>,
	   [{repository, R, _},
	    {name, Name, _}]} = _RPC, _Env) when R =:= ?USER_REPOSITORY,
					      ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    case exodm_db_yang:delete(AID, Name) of
        ok ->
            {ok, result_code(ok)};
	_Error -> %%?? more error codes internal error?
            {ok, result_code('object-not-found')}
    end;
json_rpc_({call, M, <<"delete-yang-module">>,
	   [{repository, R, _},
	    {name, Name, _}]} = _RPC, Env) when R =:= ?SYSTEM_REPOSITORY,
					      ?EXO(M) ->
    case has_root_access(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = case exodm_db_yang:delete(system, Name) of
		      ok ->
			  {ok, result_code(ok)};
		      _Error -> %%?? more error codes internal error?
			  {ok, result_code('object-not-found')}
		  end,
	    exodm_db_session:unset_trusted_proc(),
	    Res;
	false ->
	    {ok,result_code('permission-denied')}
    end;


json_rpc_({call, M, <<"list-yang-modules">>,
	   [{repository, R, _},
	    {n,N,_},
	    {previous, Prev, _}]} = _RPC, _Env) when R =:= ?USER_REPOSITORY,
						  ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    Res =
	exodm_db:in_transaction(
	  fun(_) ->
		  exodm_db_yang:list_next(AID, N, Prev)
	  end),
    {ok, [{'yang-modules', {array, Res}}]};

json_rpc_({call, M, <<"list-yang-modules">>,
	   [{repository, R, _},
	    {n, N, _},
	    {previous, Prev, _}]} = _RPC, Env) when R =:= ?SYSTEM_REPOSITORY,
						    ?EXO(M) ->
    case has_root_access(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res =
		exodm_db:in_transaction(
		  fun(_) ->
			  exodm_db_yang:list_next(system, N, Prev)
		  end),
	    exodm_db_session:unset_trusted_proc(),
	    {ok, [{'yang-modules', {array, Res}}]};
	false ->
	    {ok,result_code('permission-denied')}
    end;

json_rpc_({call, M, <<"create-yang-module">>,
	   [{repository, R, _},
	    {name, N, _},
	    {'yang-module', Y, _}]} = _RPC, Env) when R =:= ?SYSTEM_REPOSITORY,
						      ?EXO(M) ->
    case has_root_access(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res =
		case exodm_db_yang:write_system(N, Y) of
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
		end,
	    exodm_db_session:unset_trusted_proc(),
	    Res;
	false ->
	    {ok,result_code('permission-denied')}
    end;

json_rpc_({call, _, <<"provision-device">>,
	   [{'dev-id', I, _},
	    {'device-type', T, _} |
	    Opts]} = _RPC, _Env) ->
    exodm_db_device:new(exodm_db_session:get_aid(), I,
			[{'device-type', T}|kvl(Opts)]),
    {ok, result_code(ok)};

json_rpc_({call, _, <<"lookup-device">>,
	   [{'dev-id', I, _}]}, _Env) ->
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
    exodm_db_device:update(exodm_db_session:get_aid(), I, kvl(Opts)),
    {ok, result_code(ok)};


json_rpc_({call, _, <<"deprovision-devices">>,
	   [{'dev-id', DevIdList, _}]}, _Env) ->
    exodm_db_device:delete_devices(exodm_db_session:get_aid(), DevIdList),
    {ok, result_code(ok)};

json_rpc_({call, M, <<"add-config-set-members">>,
	   [{'name', CfgDataList, _},
	    {'dev-id', DevIdList, _}]} = _RPC, _Env) when ?EXO(M) ->
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

json_rpc_({call, M, <<"add-config-set-members">>,
	   [{'name', CfgDataList, _},
	    {'dev-id', DevIdList, _}]} = _RPC, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    {ok, ?catch_result(
	    exodm_db_config:add_members_to_config_sets(
	      AID, CfgDataList, DevIdList))};
    %% try [ exodm_db_config:add_config_set_members(
    %% 	    AID, CfgD, DevIdList)
    %%       || CfgD <- CfgDataList] of
    %%     _Res -> {ok, result_code(ok)}
    %% catch error:E ->
    %% 	    ?debug("RPC = ~p; ERROR = ~p~n",
    %% 		   [_RPC, {E, erlang:get_stacktrace()}]),
    %% 	    {error, E}
    %% end;

json_rpc_({call, M, <<"remove-config-set-members">>,
	   [{'name', Names, _},
	    {'dev-id', DIDs, _}]} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(remove-config-set-members) dev-id:~p names:~p~n",
           [ ?MODULE, DIDs, Names ]),
    AID = exodm_db_session:get_aid(),
    {ok, ?catch_result(exodm_db_config:remove_members_from_config_sets(
			 AID, Names, DIDs))};

json_rpc_({call, M, <<"remove-device-group-members">>,
	   [{'device-groups', Groups, _},
	    {'dev-id', DIDs, _}]} = _RPC, _Env) when ?EXO(M) ->
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

json_rpc_({call, _M, <<"provision-device">>,
	   [{DevId, DID, _}|Opts] = _Cfg} = _RPC, _Env)
  when DevId=='dev-id'; DevId=='device-id' ->
    AID = exodm_db_session:get_aid(),
    exodm_db:in_transaction(
      fun(_Db) ->
	      exodm_db_device:new(AID, DID, kvl(Opts))
      end),
    {ok, result_code(ok)};

json_rpc_({call, M, <<"list-devices">>,
	   [{n, N, _}, {previous, Prev, _}] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    Res =
	exodm_db:in_transaction(
	  fun(_) ->
		  exodm_db_device:list_next(AID, N, Prev)
	  end),
    ?debug("devices = ~p~n", [Res]),
    {ok, [{'devices',
	   {array, [ {struct, D} || D <- Res ]}
	  }]};


json_rpc_({call, M, <<"create-device-type">>,
	   [{'name', Name, _}|Opts] = _Cfg}, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    ok = exodm_db_device_type:new(AID, Name, yang_json:remove_yang_info(Opts)),
    {ok, result_code(ok)};

json_rpc_({call, M, <<"update-device-type">>,
	   [{'name', Name, _}|Opts] = _Cfg}, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    {ok, ?catch_result(
	    ok = exodm_db_device_type:update(
		   AID, Name, yang_json:remove_yang_info(Opts)))};

json_rpc_({call, M, <<"delete-device-type">>,
	   [{'name', Name, _}]}, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    {ok, ?catch_result(
	    ok = exodm_db_device_type:delete(AID, Name))};

json_rpc_({call, M, <<"list-device-types">>,
	   [{'n', N, _},
	    {'previous', Prev, _}]} = _RPC, _Env) when ?EXO(M) ->
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
    AID = exodm_db_session:get_aid(),
    {ok, GID} = exodm_db_group:new(AID, [{name, GName},
					 {url, URL}]),
    {ok, result_code(ok) ++ [{gid, to_uint32(exodm_db:group_id_value(GID))}]};

json_rpc_({call, M, <<"update-device-group">>,
	   [{'gid', GID, _}|Values] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    Values2 = lists:map(fun({'notification-url',U, _}) -> {url,U};
			   ({K,V,_}) -> {K,V}
			end, Values),
    case exodm_db_group:update(AID, GID, Values2) of
	ok -> {ok, result_code(ok)};
	{error, not_found} ->
	    {ok, result_code('object-not-found')}
    end;

json_rpc_({call, M, <<"add-device-group-members">>,
	   [{'device-groups', GIDs, _},
	    {'dev-id', DIDs, _}] = _Cfg}, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    {ok, ?catch_result(exodm_db_group:add_members_to_groups(AID, GIDs, DIDs))};

json_rpc_({call, M, <<"remove-device-group-members">>,
	   [{'device-groups', GIDs, _},
	    {'dev-id', DIDs, _}] = _Cfg}, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    {ok, ?catch_result(
	    exodm_db_group:remove_members_from_groups(AID, GIDs, DIDs))};

json_rpc_({call, M, <<"delete-device-group">>,
	   [{'gid', GID, _}] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    case exodm_db_group:delete(AID, GID) of
	ok -> {ok, result_code(ok)};
	{error, not_found} ->
	    {ok, result_code('object-not-found')}
    end;

json_rpc_({call, M, <<"list-device-groups">>,
	   [{n, N, _},
	    {previous, Prev0, _}|Tail] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    Prev = erlang:max(exodm_db:group_id_key(Prev0), <<"__last_gid">>),
    {Tab,FullPrev,F} =
	case Tail of
	    [{'device-id', DID, _}] ->
		{exodm_db_device:table(AID),
		 kvdb_conf:join_key([DID, <<"groups">>, Prev]),
		 fun(Key) ->
			 Grp = lists:last(kvdb_conf:split_key(Key)),
			 exodm_db_group:lookup(AID, Grp)
		 end};
	    [] ->
		{exodm_db_group:table(AID),
		 Prev,
		 fun(Key) ->
			 [Grp] = kvdb_conf:split_key(Key),
			 exodm_db_group:lookup(AID, Grp)
		 end}
	end,
    Res =
	exodm_db:in_transaction(
	  fun(_) ->
		  exodm_db:list_next(Tab, N, FullPrev, F)
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
	   [{'name', T, _}, {'n', N, _}, {'previous', Prev, _}] = _Params},
	  _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    Res =
	exodm_db:in_transaction(
	  fun(_) ->
		  FullNext = kvdb_conf:join_key(
			       [T, <<"devices">>, Prev]),
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
	   [{n, N, _}, {previous, Prev, _}|Tail] = _Cfg} = _RPC, _Env)
  when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    Res =
	exodm_db:in_transaction(
	  fun(_) ->
		  %% FullNext = kvdb_conf:join_key(exodm_db:account_id_key(AID),
		  %% 				Prev),
		  {Tab, FullPrev, F} =
		      case Tail of
			  [{'device-id', DID, _}] ->
			      {exodm_db_device:table(AID),
			       kvdb_conf:join_key(
				 [DID, <<"config_set">>, Prev]),
			       fun(Key) ->
				       CfgSet = lists:last(
						  kvdb_conf:split_key(Key)),
				       exodm_db_config:read_config_set(
					 AID, CfgSet)
			       end};
			  [] ->
			      {exodm_db_config:table(AID),
			       Prev,
			       fun(Key) ->
				       [CfgSet|_] =
					   kvdb_conf:split_key(Key),
				       exodm_db_config:read_config_set(
					 AID, CfgSet)
			       end}
		      end,
		  exodm_db:list_next(Tab, N, FullPrev, F)
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
	   [{'name', C, _}, {'n', N, _}, {'previous', Prev, _}] = _Params},
	  _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    Res =
	exodm_db:in_transaction(
	  fun(_) ->
		  FullNext = kvdb_conf:join_key([C, <<"members">>, Prev]),
		  exodm_db:list_next(exodm_db_config:table(AID), N, FullNext,
				     fun(Key) ->
					     lists:last(kvdb_conf:split_key(Key))
				     end)
	  end),
    ?debug("config set members = ~p~n", [Res]),
    {ok, [{'config-set-members', {array, Res}}]};

json_rpc_({call, M, <<"list-device-group-members">>,
	   [{'gid', G, _}, {'n', N, _}, {'previous', Prev, _}] = _Params},
	  _Env) when ?EXO(M) ->
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

%% Authenticated by super user 'exodm' (only allowed using SSL?)
%% create-account
json_rpc_({call, M, <<"create-account">>,
	   [{'name', Name, _Type1},
	    {'admin-user',[Admin],_Type2}| _]} = _RPC, Env)  when ?ADMIN(M) ->
    case has_root_access(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res =
		exodm_db:in_transaction(
		  fun(_Db) ->
			  case exodm_db_account:lookup_by_name(Name) of
			      [] ->
				  exodm_db_account:new([{name,Name},
							{admin, Admin}]),
				  result_code(ok);
			      [_] ->
				  result_code('object-exists')
			  end
		  end),
	    exodm_db_session:unset_trusted_proc(),
	    {ok, Res};
	false ->
	    {ok,result_code('permission-denied')}
    end;
json_rpc_({call, M, <<"update-account">>,
	   [{'name', _Name, _}|_Opts]} = _RPC, Env) when ?ADMIN(M) ->
    case has_root_access(Env) of
	true ->
	    {ok, result_code(ok)};
	false ->
	    {ok,result_code('permission-denied')}
    end;
json_rpc_({call, M, <<"delete-account">>,
	   [{'name', Name, _}|_Opts]} = _RPC, Env) when ?ADMIN(M) ->
    case has_root_access(Env) andalso (Name =/= <<"exodm">>) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res =
		exodm_db:in_transaction(
		  fun(_Db) ->
			  case exodm_db_account:lookup_by_name(Name) of
			      [Aid] ->
				  case exodm_db_account:is_empty(Aid) of
				      true ->
					  exodm_db_account:delete(Aid),
					  result_code(ok);
				      false ->
					  result_code('object-not-empty')
				  end;
			      [] ->
				  result_code('object-not-found')
			  end
		  end),			  
	    {ok, Res};
	false ->
	    {ok,result_code('permission-denied')}
    end;
json_rpc_({call, M, <<"list-accounts">>,
	   [{'n', N, _},
	    {'previous', Prev, _}]} = _RPC, Env) when ?ADMIN(M) ->
    case has_root_access(Env) of
	true ->
	    Res = lists:map(fun([{id,_},{name,Name}]) -> Name end,
			    exodm_db_account:list_accounts(N, Prev)),
	    {ok, [{'accounts', {array,Res}}]};
	false ->
	    {ok,result_code('permission-denied')}
    end;
json_rpc_({call, M, <<"create-user">>,
	   [{'uname', Name, _Type1} | Options]} , Env)  when ?ADMIN(M) ->
    case has_root_access(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res =
		exodm_db:in_transaction(
		  fun(_Db) ->
			  case exodm_db_user:lookup(Name) of
			      [] ->
				  %% Remove type info from opts
				  Opts = [{Opt, Value} || {Opt, Value, _Type} <- Options],
				  exodm_db_user:new(Name,Opts),
				  result_code(ok);
			      User ->
				  ?debug("found ~p",[User]),
				  result_code('object-exists')
			  end
		  end),
	    exodm_db_session:unset_trusted_proc(),
	    {ok, Res};
	false ->
	    {ok,result_code('permission-denied')}
    end;
json_rpc_({call, M, <<"delete-user">>,
	   [{'uname', Name, _}|_Opts]} = _RPC, Env) when ?ADMIN(M) ->
    case has_root_access(Env) andalso (Name =/= <<"exodm-admin">>) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res =
		exodm_db:in_transaction(
		  fun(_Db) ->
			  case exodm_db_user:lookup(Name) of
			      [] ->
				  result_code('object-not-found');
			      User ->
				  ?debug("found ~p",[User]),
				  %% Check if initial admin
				  exodm_db_user:delete(Name),
				  result_code(ok)
			  end
		  end),			  
	    {ok, Res};
	false ->
	    {ok,result_code('permission-denied')}
    end;
json_rpc_({call, M, <<"list-users">>,
	   [{'n', N, _},
	    {'previous', Prev, _}]} = _RPC, Env) ->
    %% In future
    %% AID = exodm_db_session:get_aid(), 
    Res = lists:map(fun(User) -> proplists:get_value(name, User) end,
		    exodm_db_user:list_users(N, Prev)),
    {ok, [{'users', {array,Res}}]};
json_rpc_(RPC, _ENV) ->
    ?info("~p:json_rpc_() Unknown RPC: ~p\n", [ ?MODULE, RPC ]),
    {ok, result_code('validation-failed')}.

%% 
%% Check if current user is a root user.
%% That is the admin user of exodm account
%% 
has_root_access(Env) ->
    [Aid0] = exodm_db_account:lookup_by_name(<<"exodm">>),
    Aid1 = exodm_db:account_id_num(Aid0),
    Aid  = exodm_db_session:get_aid(),
    Rid = exodm_db_session:get_role(),
    User = exodm_db_session:get_user(),
    ?debug("user = ~p, rootaid=~w, aid=~w, rid=~w\n",
	   [User, Aid1, Aid, Rid]),
    AccessKey = exodm_db:join_key([exodm_db:account_id_key(Aid),
				   <<"roles">>,
				   exodm_db:role_id_key(Rid),
				   <<"access">>, 
				   <<"all">>]),
    Access = exodm_db:read(<<"acct">>, AccessKey),
    ?debug("key=~p, access = ~p\n", [AccessKey, Access]),
    case Access of
	{ok, {_,_,<<"root">>}} when Aid1 =:= Aid ->
	    %% must be superuser and also using ssl / from localhost
	    proplists:get_bool(ssl, Env) 
		andalso
		  ((proplists:get_value(ip, Env) =:= {127,0,0,1})
		   orelse
		     (proplists:get_value(ip,Env) =:= {0,0,0,0,0,0,0,1}));
	_ ->
	    false
    end.


to_uint32(<<I:32>>) -> I;
to_uint32(I) when is_integer(I) -> I.

kvl([{K,V}  |Opts]) -> [{K,V}|kvl(Opts)];
kvl([{K,V,_}|Opts]) -> [{K,V}|kvl(Opts)];
kvl([]) -> [].

