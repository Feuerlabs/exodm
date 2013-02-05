%%% @author Ulf Wiger <ulf@feuerlabs.com>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%     Exosense rpc dispatching
%%% @end
%%% Created :  2012 by ??

-module(exodm_rpc_exodm).

-export([json_rpc/2,
	 exodm_admin/1]).
-include_lib("lager/include/log.hrl").

-define(USER_REPOSITORY, <<"user">>).
-define(SYSTEM_REPOSITORY, <<"system">>).

-define(EXO(M), (M==<<"exodm">> orelse M==<<"exosense">>)).
-define(ADMIN(M), (M==<<"exodm">>)).

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


%% Account
%% Authenticated by super user 'exodm' (only allowed using SSL?)
%% create-account
json_rpc_({call, M, <<"create-account">>,
	   [{'name', Name, _Type1},
	    {'admin-user',[Admin],_Type2}| _]} = _RPC, Env)  when ?ADMIN(M) ->
    case has_root_access(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = create_account(Name, Admin),
	    exodm_db_session:unset_trusted_proc(),
	    {ok, Res};
	false ->
	    {ok,result_code('permission-denied')}
    end;

json_rpc_({call, M, <<"update-account">>,
	   [{'name', _Name, _}|_Opts]} = _RPC, Env) when ?ADMIN(M) ->
    case has_root_access(Env) of
	true ->
	    %% Not implemented ???
	    {ok, result_code(ok)};
	false ->
	    {ok,result_code('permission-denied')}
    end;

json_rpc_({call, M, <<"delete-account">>,
	   [{'name', Name, _}|_Opts]} = _RPC, Env) when ?ADMIN(M) ->
    case has_root_access(Env) andalso (Name =/= <<"exodm">>) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = delete_account(Name),
	    exodm_db_session:unset_trusted_proc(),
	    {ok, Res};
	false ->
	    {ok,result_code('permission-denied')}
    end;

%% User
json_rpc_({call, M, <<"list-accounts">>,
	   [{'n', N, _},
	    {'previous', Prev, _}]} = _RPC, Env) when ?ADMIN(M) ->
    case has_root_access(Env) of
	true -> list_accounts(N, Prev);
	false -> {ok,result_code('permission-denied')}
    end;

json_rpc_({call, M, <<"create-user">>,
	   [{'uname', Name, _Type1} | Options]} , Env)  when ?ADMIN(M) ->
    case has_root_access(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = create_user(Name, Options),
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
	    Res = delete_user(Name),
	    exodm_db_session:unset_trusted_proc(),
	    {ok, Res};
	false ->
	    {ok,result_code('permission-denied')}
    end;

json_rpc_({call, M, <<"list-users">>,
	   [{'account', Account, _},
	    {'n', N, _},
	    {'previous', Prev, _}]} = _RPC, _Env) when ?EXO(M) ->
    list_users(Account, N, Prev);

json_rpc_({call, M, <<"list-users">>,
	   [{'n', N, _},
	    {'previous', Prev, _}]} = _RPC, _Env) when ?EXO(M) ->
    list_users(N, Prev);

json_rpc_({call, M, <<"add-users-to-account">>,
	   [{'account', Account, _},
	    {'role', Role, _},
	    {'unames', UNames, _}]}, Env) when ?EXO(M) ->
    {ok, ?catch_result(exodm_db_account:add_users(Account, Role, UNames,
						 has_root_access(Env)))};
json_rpc_({call, M, <<"remove-users-from-account">>,
	   [{'account', Account, _},
	    {'role', Role, _},
	    {'unames', UNames, _}]}, Env) when ?EXO(M) ->
    {ok, ?catch_result(exodm_db_account:remove_users(Account, Role, UNames,
						 has_root_access(Env)))};
json_rpc_({call, M, <<"list-account-users">>,
	   [{'n', N, _},
	    {'previous', Prev, _}]} = _RPC, _Env) when ?EXO(M) ->
    %% No account given, use session account
    AID = exodm_db_session:get_aid(), 
    list_users(AID, N, Prev);    

json_rpc_({call, M, <<"list-account-users">>,
	   [{'account', Account, _},
	    {'n', N, _},
	    {'previous', Prev, _}]} = _RPC, _Env) when ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> list_users(AID, N, Prev);
	[] -> result_code('object-not-found')
    end;


%% Config set
json_rpc_({call, M, <<"create-config-set">>,
	   [{'account', Account, _},
	    {'name', _N, _},
	    {yang, _Y, _} | _Rest] = Attrs} = _RPC, _Env) when ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> create_config_set(AID, Attrs);
	[] -> result_code('object-not-found')
    end;

json_rpc_({call, M, <<"create-config-set">>,
	   [{'name', _N, _},
	    {yang, _Y, _} | _Rest] = Attrs} = _RPC, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    create_config_set(AID, Attrs);

json_rpc_({call, M, <<"update-config-set">>,
	   [{'account', Account, _},
	    {'name', Name, _}|Attrs]} = _RPC, _Env) when ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> update_config_set(AID, Name, Attrs);
	[] -> result_code('object-not-found')
    end;

json_rpc_({call, M, <<"update-config-set">>,
	   [{'name', Name, _}|Attrs]} = _RPC, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    update_config_set(AID, Name, Attrs);

json_rpc_({call, M, <<"delete-config-set">>,
	   [{'account', Account, _},
	    {'name', Name, _}]} = _RPC, _Env) when ?EXO(M) ->
     case exodm_db_account:lookup_by_name(Account) of
	[AID] -> delete_config_set(AID, Name);
	[] -> result_code('object-not-found')
    end;

json_rpc_({call, M, <<"delete-config-set">>,
	   [{'name', Name, _}]} = _RPC, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    delete_config_set(AID, Name);

json_rpc_({call, M, <<"list-config-sets">>,
	   [{'account', Account, _},
	    {n, N, _}, 
	    {previous, Prev, _}| Tail] = _Cfg} = _RPC, _Env)
  when ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> list_config_sets(AID, N, Prev, Tail);
	[] -> result_code('object-not-found')
    end;

json_rpc_({call, M, <<"list-config-sets">>,
	   [{n, N, _}, 
	    {previous, Prev, _}|Tail] = _Cfg} = _RPC, _Env)
  when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    list_config_sets(AID, N, Prev, Tail);

json_rpc_({call, M, <<"add-config-set-members">>,
	   [{'account', Account, _},
	    {'name', CfgDataList, _},
	    {'dev-id', DevIdList, _}]} = _RPC, _Env) when ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> add_config_set_members(AID, CfgDataList, DevIdList);
	[] -> result_code('object-not-found')
    end;

json_rpc_({call, M, <<"add-config-set-members">>,
	   [{'name', CfgDataList, _},
	    {'dev-id', DevIdList, _}]} = _RPC, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    add_config_set_members(AID, CfgDataList, DevIdList);

json_rpc_({call, M, <<"remove-config-set-members">>,
	   [{'account', Account, _},
	    {'name', Names, _},
	    {'dev-id', DIDs, _}]} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(remove-config-set-members) dev-id:~p names:~p~n",
           [ ?MODULE, DIDs, Names ]),
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> remove_config_set_members(AID, Names, DIDs);
	[] -> result_code('object-not-found')
    end;
    

json_rpc_({call, M, <<"remove-config-set-members">>,
	   [{'name', Names, _},
	    {'dev-id', DIDs, _}]} = _RPC, _Env) when ?EXO(M) ->
    ?debug("~p:json_rpc(remove-config-set-members) dev-id:~p names:~p~n",
           [ ?MODULE, DIDs, Names ]),
    AID = exodm_db_session:get_aid(),
    remove_config_set_members(AID, Names, DIDs);

json_rpc_({call, M, <<"list-config-set-members">>,
	   [{'account', Account, _},
	    {'name', Name, _}, 
	    {'n', N, _}, 
	    {'previous', Prev, _}] = _Params},
	  _Env) when ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> list_config_set_members(AID, Name, N, Prev);
	[] -> result_code('object-not-found')
    end;

json_rpc_({call, M, <<"list-config-set-members">>,
	   [{'name', Name, _}, 
	    {'n', N, _}, 
	    {'previous', Prev, _}] = _Params},
	  _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    list_config_set_members(AID, Name, N, Prev);

json_rpc_({call, M, <<"push-config-set">>,
	   [{'account', Account, _},
	    {'name', Cfg, _}]} = _RPC, Env) when ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> push_config_set(AID, Cfg, Env);
	[] -> result_code('object-not-found')
    end;

json_rpc_({call, M, <<"push-config-set">>,
	   [{'name', Cfg, _}]} = _RPC, Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    push_config_set(AID, Cfg, Env);

%% Yang module
%% User repo
json_rpc_({call, M, <<"create-yang-module">>,
	   [{'account', Account, _},
	    {repository, R, _},
	    {name, N, _},
	    {'yang-module', Y, _}]} = _RPC, _Env) when R =:= ?USER_REPOSITORY,
						       ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> create_yang_module(AID, N, Y);
	[] -> result_code('object-not-found')
    end;

json_rpc_({call, M, <<"create-yang-module">>,
	   [{repository, R, _},
	    {name, N, _},
	    {'yang-module', Y, _}]} = _RPC, _Env) when R =:= ?USER_REPOSITORY,
						       ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    create_yang_module(AID, N, Y);

json_rpc_({call, M, <<"delete-yang-module">>,
	   [{'account', Account, _},
	    {repository, R, _},
	    {name, Name, _}]} = _RPC, _Env) when R =:= ?USER_REPOSITORY,
					      ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> delete_yang_module(AID, Name);
	[] -> result_code('object-not-found')
    end;

json_rpc_({call, M, <<"delete-yang-module">>,
	   [{repository, R, _},
	    {name, Name, _}]} = _RPC, _Env) when R =:= ?USER_REPOSITORY,
					      ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    delete_yang_module(AID, Name);

json_rpc_({call, M, <<"list-yang-modules">>,
	   [{'account', Account, _},
	    {repository, R, _},
	    {n,N,_},
	    {previous, Prev, _}]} = _RPC, _Env) when R =:= ?USER_REPOSITORY,
						  ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> list_yang_modules(AID, N, Prev);
	[] -> result_code('object-not-found')
    end;

json_rpc_({call, M, <<"list-yang-modules">>,
	   [{repository, R, _},
	    {n,N,_},
	    {previous, Prev, _}]} = _RPC, _Env) when R =:= ?USER_REPOSITORY,
						  ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    list_yang_modules(AID, N, Prev);

%% Yang module
%% System repo
json_rpc_({call, M, <<"create-yang-module">>,
	   [{'account', "exodm", _},
	    {repository, R, _},
	    {name, N, _},
	    {'yang-module', Y, _}]} = _RPC, Env) when R =:= ?SYSTEM_REPOSITORY,
						      ?EXO(M) ->
    case has_root_access(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = create_yang_module(system, N, Y),
	    exodm_db_session:unset_trusted_proc(),
	    Res;
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
	    Res = create_yang_module(system, N, Y),
	    exodm_db_session:unset_trusted_proc(),
	    Res;
	false ->
	    {ok,result_code('permission-denied')}
    end;

json_rpc_({call, M, <<"delete-yang-module">>,
	   [{'account', "exodm", _},
	    {repository, R, _},
	    {name, Name, _}]} = _RPC, Env) when R =:= ?SYSTEM_REPOSITORY,
					      ?EXO(M) ->
    case has_root_access(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = delete_yang_module(system, Name),
	    exodm_db_session:unset_trusted_proc(),
	    Res;
	false ->
	    {ok,result_code('permission-denied')}
    end;
json_rpc_({call, M, <<"delete-yang-module">>,
	   [{repository, R, _},
	    {name, Name, _}]} = _RPC, Env) when R =:= ?SYSTEM_REPOSITORY,
					      ?EXO(M) ->
    case has_root_access(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = delete_yang_module(system, Name),
	    exodm_db_session:unset_trusted_proc(),
	    Res;
	false ->
	    {ok,result_code('permission-denied')}
    end;


json_rpc_({call, M, <<"list-yang-modules">>,
	   [{'account', "exodm", _},
	    {repository, R, _},
	    {n, N, _},
	    {previous, Prev, _}]} = _RPC, Env) when R =:= ?SYSTEM_REPOSITORY,
						    ?EXO(M) ->
    case has_root_access(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = list_yang_modules(system, N, Prev),
	    exodm_db_session:unset_trusted_proc(),
	    Res;
	false ->
	    {ok,result_code('permission-denied')}
    end;

json_rpc_({call, M, <<"list-yang-modules">>,
	   [{repository, R, _},
	    {n, N, _},
	    {previous, Prev, _}]} = _RPC, Env) when R =:= ?SYSTEM_REPOSITORY,
						    ?EXO(M) ->
    case has_root_access(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = list_yang_modules(system, N, Prev),
	    exodm_db_session:unset_trusted_proc(),
	    {ok, [{'yang-modules', {array, Res}}]};
	false ->
	    {ok,result_code('permission-denied')}
    end;

%% Device type
json_rpc_({call, M, <<"create-device-type">>,
	   [{'account', Account, _},
	    {'name', Name, _}|Opts] = _Cfg}, _Env) when ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
		      [AID] -> create_device_type(AID, Name, Opts);
		      [] -> result_code('object-not-found')
    end;  
	
json_rpc_({call, M, <<"create-device-type">>,
	   [{'name', Name, _}|Opts] = _Cfg}, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    create_device_type(AID, Name, Opts);

json_rpc_({call, M, <<"update-device-type">>,
	   [{'account', Account, _},
	    {'name', Name, _}|Opts] = _Cfg}, _Env) when ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
		      [AID] -> update_device_type(AID, Name, Opts);
		      [] -> result_code('object-not-found')
    end;  

json_rpc_({call, M, <<"update-device-type">>,
	   [{'name', Name, _}|Opts] = _Cfg}, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    update_device_type(AID, Name, Opts);

json_rpc_({call, M, <<"delete-device-type">>,
	   [{'account', Account, _},
	    {'name', Name, _}]}, _Env) when ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
		      [AID] -> delete_device_type(AID, Name);
		      [] -> result_code('object-not-found')
    end;  

json_rpc_({call, M, <<"delete-device-type">>,
	   [{'name', Name, _}]}, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    delete_device_type(AID, Name);

json_rpc_({call, M, <<"list-device-types">>,
	   [{'account', Account, _},
	    {'n', N, _},
	    {'previous', Prev, _}]} = _RPC, _Env) when ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
		      [AID] -> list_device_types(AID, N, Prev);
		      [] -> result_code('object-not-found')
    end;  

json_rpc_({call, M, <<"list-device-types">>,
	   [{'n', N, _},
	    {'previous', Prev, _}]} = _RPC, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    list_device_types(AID, N, Prev);

json_rpc_({call, M, <<"list-device-type-members">>,
	   [{'account', Account, _},
	    {'name', Name, _}, 
	    {'n', N, _}, 
	    {'previous', Prev, _}] = _Params},
	  _Env) when ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
		      [AID] -> list_device_type_members(AID, Name, N, Prev);
		      [] -> result_code('object-not-found')
    end;

json_rpc_({call, M, <<"list-device-type-members">>,
	   [{'name', Name, _}, 
	    {'n', N, _}, 
	    {'previous', Prev, _}] = _Params},
	  _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    list_device_type_members(AID, Name, N, Prev);

%% Device group
json_rpc_({call, M, <<"create-device-group">>,
	   [{'account', Account, _},
	    {'name', GName, _},
	    {'notification-url', URL, _}] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
		      [AID] -> create_device_group(AID, GName, URL);
		      [] -> result_code('object-not-found')
    end;

json_rpc_({call, M, <<"create-device-group">>,
	   [{'name', GName, _},
	    {'notification-url', URL, _}] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    create_device_group(AID, GName, URL);

json_rpc_({call, M, <<"update-device-group">>,
	   [{'account', Account, _},
	    {'gid', GID, _}|Values] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> update_device_group(AID, GID, Values);
	[] -> result_code('object-not-found')
    end;

json_rpc_({call, M, <<"update-device-group">>,
	   [{'gid', GID, _}|Values] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    update_device_group(AID, GID, Values);

json_rpc_({call, M, <<"delete-device-group">>,
	   [{'account', Account, _},
	    {'gid', GID, _}] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> delete_device_group(AID, GID);
	[] -> result_code('object-not-found')
    end;

json_rpc_({call, M, <<"delete-device-group">>,
	   [{'gid', GID, _}] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    delete_device_group(AID, GID);

json_rpc_({call, M, <<"list-device-groups">>,
	   [{'account', Account, _},
	    {n, N, _},
	    {previous, Prev, _}|Tail] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> list_device_groups(AID, N, Prev, Tail);
	[] -> result_code('object-not-found')
    end;

json_rpc_({call, M, <<"list-device-groups">>,
	   [{n, N, _},
	    {previous, Prev, _}|Tail] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    list_device_groups(AID, N, Prev, Tail);

json_rpc_({call, M, <<"add-device-group-members">>,
	   [{'account', Account, _},
	    {'device-groups', GIDs, _},
	    {'dev-id', DIDs, _}] = _Cfg}, _Env) when ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> add_device_group_members(AID, GIDs, DIDs);
	[] -> result_code('object-not-found')
    end;
    
json_rpc_({call, M, <<"add-device-group-members">>,
	   [{'device-groups', GIDs, _},
	    {'dev-id', DIDs, _}] = _Cfg}, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    add_device_group_members(AID, GIDs, DIDs);

json_rpc_({call, M, <<"remove-device-group-members">>,
	   [{'account', Account, _},
	    {'device-groups', GIDs, _},
	    {'dev-id', DIDs, _}] = _Cfg}, _Env) when ?EXO(M) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> remove_device_group_members(AID, GIDs, DIDs);
	[] -> result_code('object-not-found')
    end;

json_rpc_({call, M, <<"remove-device-group-members">>,
	   [{'device-groups', GIDs, _},
	    {'dev-id', DIDs, _}] = _Cfg}, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    remove_device_group_members(AID, GIDs, DIDs);

json_rpc_({call, M, <<"list-device-group-members">>,
	   [{'account', Account, _},
	    {'gid', GID, _}, 
	    {'n', N, _}, 
	    {'previous', Prev, _}] = _Params}, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> list_device_group_members(AID, GID, N, Prev);
	[] -> result_code('object-not-found')
    end;

json_rpc_({call, M, <<"list-device-group-members">>,
	   [{'gid', GID, _}, 
	    {'n', N, _}, 
	    {'previous', Prev, _}] = _Params}, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    list_device_group_members(AID, GID, N, Prev);

%% Device
json_rpc_({call, _, <<"provision-device">>,
	   [{'account', Account, _},
	    {'dev-id', I, _},
	    {'device-type', T, _} |
	    Opts]} = _RPC, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> provision_device(AID, I, [{'device-type', T}|kvl(Opts)]);
	[] -> result_code('object-not-found')
    end;

json_rpc_({call, _, <<"provision-device">>,
	   [{'dev-id', I, _},
	    {'device-type', T, _} |
	    Opts]} = _RPC, _Env) ->
    AID = exodm_db_session:get_aid(),
    provision_device(AID, I, [{'device-type', T}|kvl(Opts)]);

json_rpc_({call, _M, <<"provision-device">>,
	   [{'account', Account, _},
	    {DevId, DID, _}|Opts] = _Cfg} = _RPC, _Env)
  when DevId=='dev-id'; DevId=='device-id' ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> provision_device(AID, DID, kvl(Opts));
	[] -> result_code('object-not-found')
    end;

json_rpc_({call, _M, <<"provision-device">>,
	   [{DevId, DID, _}|Opts] = _Cfg} = _RPC, _Env)
  when DevId=='dev-id'; DevId=='device-id' ->
    AID = exodm_db_session:get_aid(),
    provision_device(AID, DID, kvl(Opts));

json_rpc_({call, _, <<"lookup-device">>,
	   [{'account', Account, _},
	    {'dev-id', I, _}]}, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> lookup_device(AID, I);
	[] -> result_code('object-not-found')
    end;
    
json_rpc_({call, _, <<"lookup-device">>,
	   [{'dev-id', I, _}]}, _Env) ->
    AID = exodm_db_session:get_aid(),
    lookup_device(AID, I);
    
json_rpc_({call, _, <<"update-device">>,
	   [{'account', Account, _},
	    {'dev-id', I, _} | Opts]}, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> update_device(AID, I, Opts);
	[] -> result_code('object-not-found')
    end;

json_rpc_({call, _, <<"update-device">>,
	   [{'dev-id', I, _} | Opts]}, _Env) ->
    AID = exodm_db_session:get_aid(),
    update_device(AID, I, Opts);

json_rpc_({call, _, <<"deprovision-devices">>,
	   [{'account', Account, _},
	    {'dev-id', DevIdList, _}]}, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> deprovision_devices(AID, DevIdList);
	[] -> result_code('object-nsot-found')
    end;

json_rpc_({call, _, <<"deprovision-devices">>,
	   [{'dev-id', DevIdList, _}]}, _Env) ->
    AID = exodm_db_session:get_aid(),
    deprovision_devices(AID, DevIdList);

json_rpc_({call, M, <<"list-devices">>,
	   [{n, N, _}, 
	    {previous, Prev, _}] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    list_devices(AID, N, Prev);

json_rpc_({call, M, <<"list-devices">>,
	   [{n, N, _}, 
	    {previous, Prev, _}] = _Cfg} = _RPC, _Env) when ?EXO(M) ->
    AID = exodm_db_session:get_aid(),
    list_devices(AID, N, Prev);


json_rpc_(RPC, _ENV) ->
    ?info("~p:json_rpc_() Unknown RPC: ~p\n", [ ?MODULE, RPC ]),
    {ok, result_code('validation-failed')}.

create_account(Name, Admin) ->
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
      end).
    
delete_account(Name) ->
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
      end).
    
list_accounts(N, Prev) ->
    Res = lists:map(fun([{<<"id">>,_},{<<"name">>,Name}]) -> Name end,
		    exodm_db_account:list_accounts(N, Prev)),
    {ok, [{'accounts', {array,Res}}]}.
    
create_user(Name, Options) ->
    exodm_db:in_transaction(
      fun(_Db) ->
	      case exodm_db_user:lookup(Name) of
		  [] ->
		      %% Remove type info from opts
		      Opts = [{Opt, Value} || {Opt, Value, _Type} <- Options],
		      exodm_db_user:new(Name, Opts),
		      result_code(ok);
		  User ->
		      ?debug("found ~p",[User]),
		      result_code('object-exists')
	      end
      end).
    
delete_user(Name) ->
    exodm_db:in_transaction(
      fun(_Db) ->
	      case exodm_db_user:lookup(Name) of
		  [] ->
		      result_code('object-not-found');
		  User ->
		      ?debug("found ~p",[User]),
		      %% Check if initial admin done in exodm_db_user
		      exodm_db_user:delete(Name),
		      result_code(ok)
	      end
      end).
   
list_users(AID, N, Prev) ->
    case exodm_db_account:list_users(AID, N, Prev) of
	Users when is_list(Users) ->
	    {ok, [{'users', {array, Users}}]};
	Other ->
	    {ok, Other}
    end.
  
list_users(N, Prev) ->
    Res = lists:map(fun(User) -> proplists:get_value(name, User) end,
		    exodm_db_user:list_users(N, Prev)),
    {ok, [{'users', {array,Res}}]}.
    
create_config_set(AID, Attrs) ->
    try exodm_db_config:new_config_set(AID, kvl(Attrs)) of
	{ok, _} ->
	    ?debug("new_config_set(...) -> ok~n", []),
	    {ok, result_code(ok)}
    catch error:E ->
	    ?debug("ERROR = ~p~n",
		   [{E, erlang:get_stacktrace()}]),
	    {error, E}
    end.

update_config_set(AID, Name, Attrs) ->
    case exodm_db_config:update_config_set(AID, Name, kvl(Attrs)) of
	{ok, _} ->
	    ?debug("update_config_set(...) -> ok~n", []),
	    {ok, result_code(ok)};

        {error, Error} ->
	    ?debug("ERROR = ~p~n", [Error]),
	    {error, Error}
    end.

delete_config_set(AID, Name) ->
    case exodm_db_config:delete_config_set(AID, Name) of
	ok ->
	    ?debug("delete-config-set(...) -> ok~n", []),
	    {ok, result_code(ok)};
	{error, Error} ->
	    ?debug("delete-config-set(...) -> ERROR: ~p~n", [Error]),
	    {error, Error}
    end.
   
list_config_sets(AID, N, Prev, Tail) ->
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
	   {array, [{struct,
		     [{name, Nm},
		      {yang, Y},
		      {'notification-url', U}]} ||
		       [{name,Nm},{yang,Y},{'notification-url',U}|_] <- Res]}}]}.

add_config_set_members(AID, CfgDataList, DevIdList) ->
    try [ exodm_db_config:add_config_set_members(
	    AID, CfgD, DevIdList)
          || CfgD <- CfgDataList] of
        _Res -> {ok, result_code(ok)}
    catch error:E ->
	    ?debug("ERROR = ~p~n",
		   [{E, erlang:get_stacktrace()}]),
	    {error, E}
    end.

remove_config_set_members(AID, Names, DIDs) ->
    {ok, ?catch_result(exodm_db_config:remove_members_from_config_sets(
			 AID, Names, DIDs))}.

list_config_set_members(AID, Name, N, Prev) ->    
    Res =
	exodm_db:in_transaction(
	  fun(_) ->
		  FullNext = kvdb_conf:join_key([Name, <<"members">>, Prev]),
		  exodm_db:list_next(exodm_db_config:table(AID), N, FullNext,
				     fun(Key) ->
					     lists:last(kvdb_conf:split_key(Key))
				     end)
	  end),
    ?debug("config set members = ~p~n", [Res]),
    {ok, [{'config-set-members', {array, Res}}]}.

push_config_set(AID, Cfg, Env0) ->
    TID = proplists:get_value(transaction_id, Env0),
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
      end).
    

create_yang_module(system, N, Y) ->
    Res =  exodm_db_yang:write_system(N, Y),
    create_yang_module1(Res);
create_yang_module(AID, N, Y) ->
    %% FIXME!! we should be sure that the user can not create 
    %% exodm.yang | exosense.yang  
    %% (this will disable) the user

    %% Should the yang-module list be kept in a header-file ???
    case lists:member(N, ["exodm.yang", "exosense.yang"]) of
	true -> 
	    result_code('validation-failed');
	false ->
	    Res = exodm_db_yang:write(AID, N, Y),
	    create_yang_module1(Res)
    end.

create_yang_module1(ok) ->
    {ok, result_code(ok)};
create_yang_module1({error, {Line, Fmt, Arg } } )->
    ?info("~p:json_rpc(create-yang-module): Error: Line: ~p: ~p\n",
	  [?MODULE, Line, io_lib:fwrite(Fmt, Arg) ]),
    {ok, result_code('validation-failed')};
create_yang_module1(Err) ->
    ?info("~p:json_rpc(create-yang-module): Error: ~p\n",
	  [?MODULE, Err ]),
    {ok, result_code('validation-failed')}.

delete_yang_module(AID, Name) ->
    case exodm_db_yang:delete(AID, Name) of
        ok ->
            {ok, result_code(ok)};
	_Error -> %%?? more error codes internal error?
            {ok, result_code('object-not-found')}
    end.

list_yang_modules(AID, N, Prev) ->
    Res = exodm_db:in_transaction(
	    fun(_) ->
		    exodm_db_yang:list_next(AID, N, Prev)
	    end),
    {ok, [{'yang-modules', {array, Res}}]}.
    
create_device_type(AID, Name, Opts) ->
    ok = exodm_db_device_type:new(AID, Name, yang_json:remove_yang_info(Opts)),
    {ok, result_code(ok)}.

update_device_type(AID, Name, Opts) ->
    {ok, ?catch_result(
	    ok = exodm_db_device_type:update(
		   AID, Name, yang_json:remove_yang_info(Opts)))}.
    
delete_device_type(AID, Name) ->
    {ok, ?catch_result(
	    ok = exodm_db_device_type:delete(AID, Name))}.
    
list_device_types(AID, N, Prev) ->
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
	  }]}.

list_device_type_members(AID, Name, N, Prev) ->
    Res =
	exodm_db:in_transaction(
	  fun(_) ->
		  FullNext = kvdb_conf:join_key(
			       [Name, <<"devices">>, Prev]),
		  exodm_db:list_next(exodm_db_device_type:table(AID),
				     N, FullNext,
				     fun(Key) ->
					     lists:last(
					       kvdb_conf:split_key(Key))
				     end)
	  end),
    ?debug("device type members (~p) = ~p~n", [Name, Res]),
    {ok, [{'device-type-members', {array, Res}}]}.

create_device_group(AID, GName, URL) ->    
    {ok, GID} = exodm_db_group:new(AID, [{name, GName},
					 {url, URL}]),
    {ok, result_code(ok) ++ [{gid, to_uint32(exodm_db:group_id_value(GID))}]}.    
update_device_group(AID, GID, Values) ->
    Values2 = lists:map(fun({'notification-url',U, _}) -> {url,U};
			   ({K,V,_}) -> {K,V}
			end, Values),
    case exodm_db_group:update(AID, GID, Values2) of
	ok -> {ok, result_code(ok)};
	{error, not_found} ->
	    {ok, result_code('object-not-found')}
    end. 

delete_device_group(AID, GID) ->   
    case exodm_db_group:delete(AID, GID) of
	ok -> {ok, result_code(ok)};
	{error, not_found} ->
	    {ok, result_code('object-not-found')}
    end.
    
list_device_groups(AID, N, Prev0, Tail) ->
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
		       [{id,G},{name,Nm},{url,U}|_] <- Res]}}]}.

add_device_group_members(AID, GIDs, DIDs) ->
    {ok, ?catch_result(exodm_db_group:add_members_to_groups(AID, GIDs, DIDs))}.
 
remove_device_group_members(AID, GIDs, DIDs) ->   
    {ok, ?catch_result(
	    exodm_db_group:remove_members_from_groups(AID, GIDs, DIDs))}.
   
list_device_group_members(AID, GID, N, Prev) ->   
    Res =
	exodm_db:in_transaction(
	  fun(_) ->
		  exodm_db_group:list_devices(AID,GID,N,Prev)
		  %% FullNext = kvdb_conf:join_key([exodm_db:account_id_key(AID),
		  %% 				 C, <<"members">>, Prev]),
		  %% exodm_db:list_next(exodm_db_config:table(AID), N, FullNext,
		  %% 		     fun(Key) ->
		  %% 			     lists:last(kvdb_conf:split_key(Key))
		  %% 		     end)
	  end),
    ?debug("device group members = ~p~n", [Res]),
    {ok, [{'device-group-members', {array, Res}}]}.

provision_device(AID, I, Opts) -> 
    exodm_db:in_transaction(
      fun(_Db) ->
	      exodm_db_device:new(AID, I, Opts)
      end),
    {ok, result_code(ok)}. %% Always ok ??? 

lookup_device(AID, I) ->   
    Res = exodm_db_device:lookup(AID, I),
    case Res of
	[] ->
	    {ok, result_code('device-not-found')};
	[_|_] ->
	    {ok, result_code(ok) ++
		 [{devices, {array, [{struct, Res} || Res =/= []]}}]}
    end.
 
update_device(AID, I, Opts) ->   
    exodm_db_device:update(AID, I, kvl(Opts)),
    {ok, result_code(ok)}.
   
deprovision_devices(AID, DevIdList)  -> 
    exodm_db_device:delete_devices(AID, DevIdList),
    {ok, result_code(ok)}.
    
list_devices(AID, N, Prev) ->
    Res =
	exodm_db:in_transaction(
	  fun(_) ->
		  exodm_db_device:list_next(AID, N, Prev)
	  end),
    ?debug("devices = ~p~n", [Res]),
    {ok, [{'devices',
	   {array, [ {struct, D} || D <- Res ]}
	  }]}.    
%% 
%% Check if current user is a root user.
%% That is the admin user of exodm account
%% 
has_root_access(Env) ->
    [Aid0] = exodm_db_account:lookup_by_name(<<"exodm">>),
    Aid1 = exodm_db:account_id_num(Aid0),
    Aid  = exodm_db_session:get_aid(),
    RName = exodm_db_session:get_role(),
    User = exodm_db_session:get_user(),
    ?debug("user = ~p, rootaid=~w, aid=~w, rname=~w\n",
	   [User, Aid1, Aid, RName]),
    AccessKey = exodm_db:join_key([exodm_db:account_id_key(Aid),
				   <<"roles">>,
				   exodm_db:encode_id(RName),
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

