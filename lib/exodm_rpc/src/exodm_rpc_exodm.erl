%%% @author Ulf Wiger <ulf@feuerlabs.com>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%     Exosense rpc dispatching
%%% @end
%%% Created :  2012 by ??

-module(exodm_rpc_exodm).

-include("exodm.hrl").
-include("log.hrl").
-include_lib("kvdb/include/kvdb_conf.hrl").

-export([json_rpc/2,
	 exodm_admin/1]).

-define(USER_REPOSITORY, <<"user">>).
-define(SYSTEM_REPOSITORY, <<"system">>).

-define(catch_result(Expr),
    result_code(try Expr
    catch
	    error:?PERMISSION_DENIED = E -> E;
	    error:?VALIDATION_FAILED = E -> E;
	    error:?OBJECT_EXISTS     = E -> E;
	    error:?DEVICE_NOT_FOUND  = E -> E;
	    error:?OBJECT_NOT_FOUND  = E -> E;
	    error:?OBJECT_NOT_EMPTY  = E -> E;
	    error:?ACCOUNT_NOT_SPECIFIED = E -> E
end)).

%%--------------------------------------------------------------------
%% @doc
%% Checks if User is the system admin.
%%
%% @end
%%--------------------------------------------------------------------
-spec exodm_admin(UID::binary()) -> boolean().

exodm_admin(?EXODM_ADMIN) -> true;
exodm_admin(_Other) -> false.
    

result_code(Code) when is_atom(Code) ->
    [{result, atom_to_binary(Code, latin1)}];
result_code(Code) when is_binary(Code) ->
    [{result, Code}].


%%--------------------------------------------------------------------
%% @doc
%% Rpc dispatcher.
%%
%% @end
%%--------------------------------------------------------------------
-spec json_rpc({call, Module::binary(), 
                RPC::binary(), 
                InPuts::list(tuple())}, 
               Env::list({Key::atom(), Value::term()})) ->
                          {ok, Result::term()}.

json_rpc(RPC = {_, ?EXODM, Rpc, InputList}, Env) ->
    ?debug("rpc ~p~nenv~p~n", [RPC, Env]),

    %% Access control
    User = proplists:get_value(user,Env),
    case {lists:member(Rpc, ?ROOT_ACCESS_RPCS), is_root(User)} of
        %% Account rpcs must be treated specially since there
        %% is no 'account' input attribute
        %% The user must be root
        {true, true} -> 
            case has_root_env(Env) of
                true ->
                    exodm_db_session:set_trusted_proc(),
                    Res = do_json_rpc(not_set, RPC, Env),
                    exodm_db_session:unset_trusted_proc(),
                    Res;
                false ->
                    ?debug("not root env", []),
                    {ok, result_code(?PERMISSION_DENIED)}
            end;
        {true, false} ->
            ?debug("not root", []),
            {ok, result_code(?PERMISSION_DENIED)};
        {false, IsRoot} ->
	    case {lists:member(Rpc, ?USER_ADM_RPCS), 
		  compare_user(InputList, Env)} of
		{true, equal} ->
		    %% These user rpcs don't need account
		    %% and can be done by "yourself"
		    do_json_rpc(not_set, RPC, Env);
		{_, _} ->
		    %% If not equal account is needed for access control
		    case find_account(InputList, Env) of
			undefined -> 
			    ?debug("no account", []),
			    {ok, result_code(?ACCOUNT_NOT_SPECIFIED)};
			AID ->
			    ?debug("account ~p", [AID]),
			    case has_access(AID, User, Rpc, IsRoot) of
				true  -> 
				    ?debug("user ~p has permission for rpc ~p",
					   [User, Rpc]),
				    do_json_rpc(AID, RPC, Env);
				false ->  
				    ?debug("no permission for user ~p", [User]),
				    {ok, result_code(?PERMISSION_DENIED)}
			    end
		    end
	    end
    end.

compare_user(InputList, Env) ->
    EnvUser = proplists:get_value(user, Env, undefined),
    case lists:keyfind('uname', 1, InputList) of
        {'uname', EnvUser, _} -> equal;
	_ -> not_equal
    end.
 
%% If account isn't found in InputList Env is used.
%% But Env account is set to first of users accounts
%% Is that OK ??
%% Or should a user that isn't of Account/Name type
%% always have an account where it is initial-admin
%% and that be choosen ??
find_account(InputList, Env) ->
    case lists:keyfind('account', 1, InputList) of
        {'account', Account, _} -> aname2aid(Account);
        false -> find_account1(InputList, Env)
    end.

find_account1(InputList, Env) ->
    case lists:keyfind('uname', 1, InputList) of
	{'uname', UName, _} ->
	    %% See if user is of Account/Name type,
	    case exodm_db:nc_key_split(exodm_db:decode_id(UName)) of
		[Account, _Name] -> aname2aid(Account);
		_ -> proplists:get_value(aid, Env, undefined)
	    end;
	false ->
	    proplists:get_value(aid, Env, undefined)
    end.

aname2aid(Account) ->
    case exodm_db_account:lookup_by_name(Account) of
	AID when is_binary(AID) -> AID;
	false -> undefined
    end.

has_access(_AID, _User, _Rpc, true) ->
    true;
has_access(AID, User, Rpc, false) ->
    case exodm_db_account:list_user_roles(AID, User) of
        [] -> 
            ?debug("no roles for user ~p", [User]),
            false;
        Roles ->
            ?debug("user ~p has roles ~p", [User, Roles]),
            exodm_db_account:rpc_permission(Rpc, Roles)
    end.
    
%% Account
%% Authenticated by super user 'exodm' (only allowed using SSL?)
%% create-account
do_json_rpc(not_set, {call, ?EXODM, ?RPC_CREATE_ACCOUNT,
	   [{'name', Name, _Type1}| Opts]} = _RPC, Env) ->
    case has_root_env(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = create_account(Name, Opts),
	    exodm_db_session:unset_trusted_proc(),
	    Res;
	false ->
	    {ok, result_code(?PERMISSION_DENIED)}
    end;

do_json_rpc(not_set, {call, ?EXODM, ?RPC_UPDATE_ACCOUNT,
	   [{'name', _Name, _}|_Opts]} = _RPC, Env) ->
    case has_root_env(Env) of
	true ->
	    %% Not implemented ???
	    {ok, result_code(ok)};
	false ->
	    {ok, result_code(?PERMISSION_DENIED)}
    end;

do_json_rpc(not_set, {call, ?EXODM, ?RPC_DELETE_ACCOUNT,
	   [{'name', Name, _}|_Opts]} = _RPC, Env) ->
    case has_root_env(Env) andalso (Name =/= <<"exodm">>) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = delete_account(Name),
	    exodm_db_session:unset_trusted_proc(),
	    Res;
	false ->
	    {ok, result_code(?PERMISSION_DENIED)}
    end;

do_json_rpc(_AID, {call, _, ?RPC_LOOKUP_ACCOUNT,
	   [{'name', Name, _}|_Tail]}, Env) ->
    case has_root_env(Env) of
	true -> lookup_account(Name);
	false -> {ok, result_code(?PERMISSION_DENIED)}
    end;

do_json_rpc(not_set, {call, ?EXODM, ?RPC_LIST_ACCOUNTS,
	   [{'n', N, _},
	    {'previous', Prev, _}]} = _RPC, Env) ->
    case has_root_env(Env) of
	true -> list_accounts(N, Prev);
	false -> {ok, result_code(?PERMISSION_DENIED)}
    end;

do_json_rpc(AID, {call, ?EXODM, ?RPC_LIST_ACCOUNT_ROLES,
	   [{'account', _Account, _}, %% Already parsed
	    {'n', N, _},
	    {'previous', Prev, _}|_Tail]} = _RPC, _Env) ->
    list_account_roles(AID, N, Prev);    

%% User
do_json_rpc(_AID, {call, ?EXODM, ?RPC_CREATE_USER,
	   [{'uname', Name, _Type1} | Options]}, _Env) ->
    create_user(Name, Options);

do_json_rpc(_AID, {call, ?EXODM, ?RPC_DELETE_USER,
	   [{'uname', Name, _}|_Opts]} = _RPC, _Env) 
  when Name =/= ?EXODM_ADMIN ->
    delete_user(Name);

do_json_rpc(_AID, {call, ?EXODM, ?RPC_UPDATE_USER,
	   [{'uname', Name, _Type1} | Options]}, _Env)  
  when Name =/= ?EXODM_ADMIN ->
    update_user(Name, Options);

do_json_rpc(_AID, {call, ?EXODM, ?RPC_UPDATE_USER,
	   [{'uname', Name, _Type1} | Options]}, Env) ->
    case is_root(Env) of
	true -> update_user(Name, Options);
	false -> {ok, result_code(?PERMISSION_DENIED)}
    end;

do_json_rpc(_AID, {call, _, ?RPC_LOOKUP_USER,
	   [{'uname', Name, _}|_Tail]}, _Env) ->
    lookup_user(Name);
    
do_json_rpc(_AID, {call, ?EXODM, ?RPC_LIST_USER_ACCOUNTS,
	   [{'uname', Name, _}|_Tail]} = _RPC, _Env) ->
    list_user_accounts(Name);

do_json_rpc(AID, {call, ?EXODM, ?RPC_LIST_USERS,
	   [{'n', N, _},
	    {'previous', Prev, _},
	    {'direction', Dir, _},
            {'account', _Account, _}]} = _RPC, _Env) ->
    %% Account users
    list_users(AID, N, Prev, Dir);

do_json_rpc(_AID, {call, ?EXODM, ?RPC_LIST_USERS,
	   [{'n', N, _},
	    {'previous', Prev, _},
	    {'direction', Dir, _} |_Tail]} = _RPC, _Env) ->
    %% All users
    list_users(N, Prev, Dir);

do_json_rpc(_AID, {call, ?EXODM, ?RPC_ADD_ACCOUNT_USERS,
	   [{'account', Account, _},
	    {'role', Role, _},
	    {'unames', UNames, _}|_Tail]}, Env) ->
    {ok, ?catch_result(exodm_db_account:add_users(Account, Role, UNames,
						 has_root_env(Env)))};
do_json_rpc(_AID, {call, ?EXODM, ?RPC_REMOVE_ACCOUNT_USERS,
	   [{'account', Account, _},
	    {'role', Role, _},
	    {'unames', UNames, _}|_Tail]}, Env) ->
    {ok, ?catch_result(exodm_db_account:remove_users(Account, Role, UNames,
						 has_root_env(Env)))};
do_json_rpc(_AID, {call, ?EXODM, ?RPC_REMOVE_ACCOUNT_USER,
	   [{'account', Account, _},
	    {'uname', UName, _}|_Tail]}, Env) ->
    {ok, ?catch_result(exodm_db_account:remove_user(Account, UName,
						 has_root_env(Env)))};
do_json_rpc(AID, {call, ?EXODM, ?RPC_LIST_ACCOUNT_USERS,
	   [{'account', _Account, _}, %% Already parsed
	    {'n', N, _},
	    {'previous', Prev, _},
	    {'direction', Dir, _} |_Tail]} = _RPC, _Env) ->
    list_users(AID, N, Prev, Dir);    


%% Config set
do_json_rpc(AID, {call, ?EXODM, ?RPC_CREATE_CONFIG_SET,
	   [{'name', _N, _},
	    {yang, _Y, _} | _Rest] = Attrs} = _RPC, _Env) ->
    create_config_set(AID, Attrs);

do_json_rpc(AID, {call, ?EXODM, ?RPC_UPDATE_CONFIG_SET,
	   [{'name', Name, _}|Attrs]} = _RPC, _Env) ->
    update_config_set(AID, Name, Attrs);

do_json_rpc(AID, {call, ?EXODM, ?RPC_READ_CONFIG_SET_DATA,
		[{'name', Name, _}, {'area', Area, _} | _] = _Attrs} = _RPC,
	  _Env) ->
    read_config_set_data(AID, Name, Area);

do_json_rpc(AID, {call, ?EXODM, ?RPC_DELETE_CONFIG_SET,
	   [{'name', Name, _}|_Tail]} = _RPC, _Env) ->
    delete_config_set(AID, Name);

do_json_rpc(AID, {call, ?EXODM, ?RPC_LIST_CONFIG_SETS,
	   [{n, N, _}, 
	    {previous, Prev, _}|Tail] = _Cfg} = _RPC, _Env) ->
    list_config_sets(AID, N, Prev, Tail);

do_json_rpc(AID, {call, ?EXODM, ?RPC_ADD_CONFIG_SET_MEMBERS,
	   [{'name', CfgDataList, _},
	    {'device-id', DevIdList, _}|_Tail]} = _RPC, _Env) ->
    add_config_set_members(AID, CfgDataList, DevIdList);

do_json_rpc(AID, {call, ?EXODM, ?RPC_REMOVE_CONFIG_SET_MEMBERS,
	   [{'name', Names, _},
	    {'device-id', DIDs, _}|_Tail]} = _RPC, _Env) ->
    remove_config_set_members(AID, Names, DIDs);
        
do_json_rpc(AID, {call, ?EXODM, ?RPC_LIST_CONFIG_SET_MEMBERS,
	   [{'name', Name, _}, 
	    {'n', N, _}, 
	    {'previous', Prev, _}|_Tail] = _Params},
	  _Env) ->
    list_config_set_members(AID, Name, N, Prev);

do_json_rpc(AID, {call, ?EXODM, ?RPC_ADD_NOTIFICATION_URLS,
		[{'device-id', DID, _},
		 {'urls', URLs, _}]}, _Env) ->
    add_notification_urls(AID, DID, URLs);

do_json_rpc(AID, {call, ?EXODM, ?RPC_REMOVE_NOTIFICATION_URLS,
		[{'device-id', DID, _},
		 {'config-sets', ConfigSets, _}]}, _Env) ->
    del_notification_urls(AID, DID, ConfigSets);

do_json_rpc(AID, {call, ?EXODM, ?RPC_PUSH_CONFIG_SET,
	   [{'name', Cfg, _}|_Tail]} = _RPC, Env) ->
    push_config_set(AID, Cfg, Env);
    

%% Yang module
%% User repo
do_json_rpc(AID, {call, ?EXODM, ?RPC_CREATE_YANG_MODULE,
	   [{repository, ?USER_REPOSITORY, _},
	    {name, N, _},
	    {'yang-module', Y, _}|_Tail]} = _RPC, Env) ->
    ?debug("aid ~p, n ~p", [AID, N]),
    create_yang_module(AID, N, yang_data(Y, Env));

do_json_rpc(AID, {call, ?EXODM, ?RPC_DELETE_YANG_MODULE,
	   [{repository, ?USER_REPOSITORY, _},
	    {name, Name, _}|_Tail]} = _RPC, _Env) ->
    delete_yang_module(AID, Name);

do_json_rpc(AID, {call, ?EXODM, ?RPC_LOOKUP_YANG_MODULE,
	   [{repository, ?USER_REPOSITORY, _},
	    {name,Name,_}|_Tail]} = _RPC, _Env) ->
    lookup_yang_module(AID, Name);

do_json_rpc(AID, {call, ?EXODM, ?RPC_LIST_YANG_MODULES,
	   [{repository, ?USER_REPOSITORY, _},
	    {n,N,_},
	    {previous, Prev, _},
	    {'direction', Dir, _} |_Tail]} = _RPC, _Env) ->
    list_yang_modules(AID, N, Prev, Dir);

do_json_rpc(AID, {call, ?EXODM, ?RPC_LIST_EXEC_PERMISSION,
	   [{repository, ?USER_REPOSITORY, _},
	    {modulename, MName, _},
            {rpcname, RName, _}|_Tail]} = _RPC, _Env) ->
    list_exec_permission(AID, MName, RName);

%% Yang module
%% System repo
do_json_rpc(_AID, {call, ?EXODM, ?RPC_CREATE_YANG_MODULE,
	   [{repository, ?SYSTEM_REPOSITORY, _},
	    {name, N, _},
	    {'yang-module', Y, _}|_Tail]} = _RPC, Env) ->
    case has_root_env(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = create_yang_module(system, N, yang_data(Y, Env)),
	    exodm_db_session:unset_trusted_proc(),
	    Res;
	false ->
	    {ok,result_code(?PERMISSION_DENIED)}
    end;

do_json_rpc(_AID, {call, ?EXODM, ?RPC_DELETE_YANG_MODULE,
	   [{repository, ?SYSTEM_REPOSITORY, _},
	    {name, Name, _}|_Tail]} = _RPC, Env) ->
    case has_root_env(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = delete_yang_module(system, Name),
	    exodm_db_session:unset_trusted_proc(),
	    Res;
	false ->
	    {ok,result_code(?PERMISSION_DENIED)}
    end;

do_json_rpc(AID, {call, ?EXODM, ?RPC_LOOKUP_YANG_MODULE,
	   [{repository, ?SYSTEM_REPOSITORY, _},
	    {name,Name,_}|_Tail]} = _RPC, Env) ->
        case has_root_env(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = lookup_yang_module(AID, Name),
	    exodm_db_session:unset_trusted_proc(),
	    {ok, [{'yang-modules', {array, Res}}]};
	false ->
	    {ok,result_code(?PERMISSION_DENIED)}
    end;

do_json_rpc(_AID, {call, ?EXODM, ?RPC_LIST_YANG_MODULES,
	   [{repository, ?SYSTEM_REPOSITORY, _},
	    {n, N, _},
	    {previous, Prev, _},
	    {'direction', Dir, _} |_Tail]} = _RPC, Env) ->
    case has_root_env(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = list_yang_modules(system, N, Prev, Dir),
	    exodm_db_session:unset_trusted_proc(),
	    {ok, [{'yang-modules', {array, Res}}]};
	false ->
	    {ok,result_code(?PERMISSION_DENIED)}
    end;

%% Device type
do_json_rpc(AID, {call, ?EXODM, ?RPC_CREATE_DEVICE_TYPE,
	   [{'name', Name, _}|Opts] = _Cfg}, _Env) ->
    create_device_type(AID, Name, Opts);

do_json_rpc(AID, {call, ?EXODM, ?RPC_UPDATE_DEVICE_TYPE,
	   [{'name', Name, _}|Opts] = _Cfg}, _Env) ->
    update_device_type(AID, Name, Opts);

do_json_rpc(AID, {call, ?EXODM, ?RPC_DELETE_DEVICE_TYPE,
	   [{'name', Name, _}|_Tail]}, _Env) ->
    delete_device_type(AID, Name);

do_json_rpc(AID, {call, ?EXODM, ?RPC_LIST_DEVICE_TYPES,
	   [{'n', N, _},
	    {'previous', Prev, _}|_Tail]} = _RPC, _Env) ->
    list_device_types(AID, N, Prev);

do_json_rpc(AID, {call, ?EXODM, ?RPC_LIST_DEVICE_TYPE_MEMBERS,
	   [{'name', Name, _}, 
	    {'n', N, _}, 
	    {'previous', Prev, _}|_Tail] = _Params},
	  _Env) ->
    list_device_type_members(AID, Name, N, Prev);

%% Device group
do_json_rpc(AID, {call, ?EXODM, ?RPC_CREATE_DEVICE_GROUP,
	   [{'name', GName, _},
	    {'notification-url', URL, _}|_Tail] = _Cfg} = _RPC, _Env) ->
    create_device_group(AID, GName, URL);

do_json_rpc(AID, {call, ?EXODM, ?RPC_UPDATE_DEVICE_GROUP,
	   [{'group-id', GID, _}|Values] = _Cfg} = _RPC, _Env) ->
    update_device_group(AID, GID, Values);

do_json_rpc(AID, {call, ?EXODM, ?RPC_DELETE_DEVICE_GROUP,
	   [{'group-id', GID, _}|_Tail] = _Cfg} = _RPC, _Env) ->
    delete_device_group(AID, GID);

do_json_rpc(AID, {call, ?EXODM, ?RPC_LIST_DEVICE_GROUPS,
	   [{n, N, _},
	    {previous, Prev, _}|Tail] = _Cfg} = _RPC, _Env) ->
    list_device_groups(AID, N, Prev, Tail);

do_json_rpc(AID, {call, ?EXODM, ?RPC_ADD_DEVICE_GROUP_MEMBERS,
	   [{'group-id', GIDs, _},
	    {'device-id', DIDs, _}|_Tail] = _Cfg}, _Env) ->
    add_device_group_members(AID, GIDs, DIDs);

do_json_rpc(AID, {call, ?EXODM, ?RPC_REMOVE_DEVICE_GROUP_MEMBERS,
	   [{'group-id', GIDs, _},
	    {'device-id', DIDs, _}|_Tail] = _Cfg}, _Env) ->
    remove_device_group_members(AID, GIDs, DIDs);

do_json_rpc(AID, {call, ?EXODM, ?RPC_LIST_DEVICE_GROUP_MEMBERS,
	   [{'group-id', GID, _}, 
	    {'n', N, _}, 
	    {'previous', Prev, _}|_Tail] = _Params}, _Env) ->
    list_device_group_members(AID, GID, N, Prev);

%% Device
do_json_rpc(AID, {call, _, ?RPC_PROVISION_DEVICE,
	   [{'device-id', I, _},
	    {'device-type', T, _} |
	    Opts]} = _RPC, _Env) ->
    provision_device(AID, I, [{'device-type', T}|kvl(Opts)]);

do_json_rpc(AID, {call, _, ?RPC_PROVISION_DEVICE,
	   [{'device-id', DID, _}|Opts] = _Cfg} = _RPC, _Env) ->
    provision_device(AID, DID, kvl(Opts));

do_json_rpc(AID, {call, _, ?RPC_LOOKUP_DEVICE,
	   [{'device-id', I, _}|_Tail]}, _Env) ->
    lookup_device(AID, I);
    
do_json_rpc(AID, {call, _, ?RPC_LOOKUP_DEVICE_ATTRIBUTES,
	   [{'device-id', I, _}, {'attributes', Attrs, _} |_Tail]}, _Env) ->
    lookup_device_attributes(AID, I, Attrs);
    
do_json_rpc(AID, {call, _, ?RPC_UPDATE_DEVICE,
	   [{'device-id', I, _} | Opts]}, _Env) ->
    update_device(AID, I, Opts);

do_json_rpc(AID, {call, _, ?RPC_DEPROVISION_DEVICES,
	   [{'device-id', DevIdList, _}|_Tail]}, _Env) ->
    deprovision_devices(AID, DevIdList);

do_json_rpc(AID, {call, ?EXODM, ?RPC_LIST_DEVICES,
	   [{n, N, _}, {'previous', Prev, _} |_Tail] = _Cfg} = _RPC, _Env) ->
    list_devices(AID, N, Prev);

do_json_rpc(AID, {call, ?EXODM, ?RPC_LIST_DEVICES_ATTRIBUTES,
		  [{n, N, _}, 
		   {'previous', Prev, _}, 
		   {'attributes', Attrs, _}, 
		   {'pattern', Pattern, _},
		   {'direction', Dir, _}
		   |_Tail] = _Cfg} = _RPC, _Env) ->
    list_devices_attributes(AID, N, Prev, Attrs, Pattern, Dir);

%% Mblox access parameter configuration
do_json_rpc(AID, {call, ?EXODM, ?RPC_SET_MBLOX_PARAMETERS,
		[{account, _Acct, _},
		 {'application-id', AppID, _},
		 {'originating-msisdn', OrigMsisdn, _},
		 {'consumer-key', ConsumerKey, _},
		 {'secret-key', SecretKey, _} | Rest]}, _Env) ->
    Tree = [{<<"application-id">>, [], AppID},
	    {<<"originating-msisdn">>, [], OrigMsisdn},
	    {<<"consumer-key">>, [], ConsumerKey},
	    {<<"secret-key">>, [], SecretKey} |
	    case lists:keyfind('token', 1, Rest) of
		{_, Token, _} -> [{'token', [], Token}];
		false -> []
	    end],
    kvdb_conf:write_tree(acct, kvdb_conf:join_key(
				 exodm_db:account_id_key(AID), <<"mblox">>),
			 Tree),
    exodm_rpc_push_mblox:account_updated(AID),
    {ok, result_code(ok)};

do_json_rpc(AID, {call, ?EXODM, ?RPC_GET_MBLOX_PARAMETERS,
		[{account, _Acct, _}]}, _Env) ->
    Key = kvdb_conf:join_key(
	    exodm_db:account_id_key(AID), <<"mblox">>),
    case kvdb_conf:read_tree(acct, Key) of
	[] ->
	    {ok, result_code(?OBJECT_NOT_FOUND)};
	Tree ->
	    {ok, [result_code(ok) | [ {K, V} || {K, _, V} <- Tree]]}
    end;

do_json_rpc(AID, {call, ?EXODM, ?RPC_DELETE_MBLOX_PARAMETERS,
		[{account, _Acct, _}]}, _Env) ->
    K = kvdb_conf:join_key(
	  exodm_db:account_id_key(AID), <<"mblox">>),
    kvdb_conf:delete_tree(acct, K),
    {ok, result_code(ok)};

%% Failure case
do_json_rpc(AID, RPC, _ENV) ->
    ?info("~p:do_json_rpc() aid ~p, unknown RPC: ~p\n", [ ?MODULE, AID, RPC ]),
    {ok, result_code(?VALIDATION_FAILED)}.

%%--------------------------------------------------------------------
%%
%% Internal functions
%%
%%--------------------------------------------------------------------
create_account(Name, Options) ->
    Opts = [{Opt, Value} || {Opt, Value, _Type} <- Options],
    case exodm_db_account:new(Name, Opts) of
	{ok, Admin} -> 
	    {ok, result_code(ok) ++ [{'account-admin', Admin}]};
	Res -> {ok, result_code(Res)}
    end.
    
delete_account(Name) ->
    {ok, ?catch_result(exodm_db_account:delete(Name))}.
    
lookup_account(Name) ->   
    Res = exodm_db_account:lookup(exodm_db_account:lookup_by_name(Name)),
    case Res of
	[] ->
	    {ok, result_code(?OBJECT_NOT_FOUND)};
	[_|_] ->
	    {ok, result_code(ok) ++
		 [{'accounts', {array, [{struct, Res} || Res =/= []]}}]}
    end.
list_accounts(N, Prev) ->
    Res = lists:map(fun([{<<"id">>,_},{<<"name">>,Name}]) -> Name end,
		    exodm_db_account:list_accounts(N, Prev)),
    {ok, result_code(ok) ++ [{'accounts', {array, Res}}]}.
    
list_account_roles(AID, N, Prev) ->
    Res = [binary_to_atom(Role, latin1) || 
              Role <- exodm_db_account:list_roles(AID, N, Prev)],
    {ok, result_code(ok) ++ [{'roles', {array, Res}}]}.
    
create_user(Name, Options) ->
    %% Remove type info from opts
    Opts = [{Opt, Value} || {Opt, Value, _Type} <- Options],
    {ok, ?catch_result(exodm_db_user:new(Name, Opts))}.
    
delete_user(Name) ->
    {ok, ?catch_result(exodm_db_user:delete(Name, false))}.
   
update_user(Name, Options) ->
    %% Remove type info from opts
    Opts = [{Opt, Value} || {Opt, Value, _Type} <- Options],
    {ok, ?catch_result(exodm_db_user:update(Name, Opts))}.
   
lookup_user(Name) ->   
    Res = exodm_db_user:lookup(Name),
    case Res of
	[] ->
	    {ok, result_code(?OBJECT_NOT_FOUND)};
	[_|_] ->
	    ?debug("lookup_user: found ~p", [Res]),
	    {ok, result_code(ok) ++
		 [{'users', {array, [{struct, Res} || Res =/= []]}}]}
    end.

list_users(AID, N, Prev, Dir) ->
    case exodm_db_account:list_users_with_roles(AID, N, Prev, Dir) of
	[] ->
	    {ok, result_code(ok) ++ [{'users', {array, []}}]};
	Users when is_list(Users) ->
	    {ok, result_code(ok) ++ 
		[{'users', {array, [{struct, [{'name', Name},
		                              {'roles',  Roles}]} ||
		    {Name, Roles} <- Users]}}]};
	Other ->
	    {ok, Other}
    end.

list_users(N, Prev, Dir) ->
    Res = lists:map(fun(User) -> proplists:get_value(name, User) end,
		    exodm_db_user:list_users(N, Prev, Dir)),
    {ok, result_code(ok) ++ [{'users', {array,Res}}]}.

list_user_accounts(Name) ->  
    case exodm_db_user:list_roles(Name) of
	[] ->
	    {ok, result_code(ok) ++ [{'accounts', {array, []}}]};
	Accounts when is_list(Accounts) ->
	    %% Cheating with roles !!! FIXME
	    {ok, result_code(ok) ++
		[{'accounts', 
		   {array, 
		    [{struct, [{'name', exodm_db_account:lookup_name(Account)},
			       {'roles',  [Role]}]} ||
			{Account, Role} <- Accounts]}}]};
	Other ->
	    {ok, Other}
    end.
    
    
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

read_config_set_data(AID, Name, Area) ->
    case exodm_db_config:read_config_set_values(AID, Name, Area) of
	{ok, #conf_tree{} = CT} ->
	    Values = [{K,V} || {K,_,V} <- kvdb_conf:flatten_tree(
					    CT#conf_tree{root = <<>>})],
	    {ok, [{'values', {struct, Values}}]};
	_Other ->
	    {ok, result_code(?OBJECT_NOT_FOUND)}
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
		      case lists:keyfind('device-id', 1, Tail) of
			  {'device-id', DID, _} ->
			      {exodm_db_device:table(AID),
			       kvdb_conf:join_key(
				 [DID, <<"config_set">>, Prev]),
			       fun(Key) ->
				       CfgSet = lists:last(
						  kvdb_conf:split_key(Key)),
				       exodm_db_config:read_config_set(
					 AID, CfgSet)
			       end};
			  false ->
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
    {ok, result_code(ok) ++
	[{'config-sets',
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
    {ok, result_code(ok) ++ [{'config-set-members', {array, Res}}]}.

add_notification_urls(AID, DID, URLs) ->
    Res = exodm_db:in_transaction(
	    fun(_) ->
		    [exodm_db_device:add_request_url(AID, DID, CS, URL)
		     || {CS, URL} <- URLs],
		    ok
	    end),
    ?debug("add-notification-urls = ~p~n", [Res]),
    {ok, result_code(ok)}.

del_notification_urls(AID, DID, ConfigSets) ->
    Res = exodm_db:in_transaction(
	    fun(_) ->
		    [exodm_db_device:delete_request_url(AID, DID, CS)
		     || CS <- ConfigSets],
		    ok
	    end),
    ?debug("delete-notification-urls = ~p~n", [Res]),
    {ok, result_code(ok)}.

push_config_set(AID, Cfg, Env0) ->
    TID = proplists:get_value(transaction_id, Env0),
    User = exodm_db_session:get_user(),
    exodm_db:in_transaction(
      fun(Db) ->
	      case exodm_db_config:list_config_set_members(AID, Cfg) of
		  [_|_] = Devices ->
		      {ok, _Yang} = exodm_db_config:get_yang_spec(AID, Cfg),
		      {ok, Ref} = exodm_db_config:cache_values(AID, Cfg),
		      RPC = {call, <<"exodm">>,
			     <<"push-config-set">>,
			     [{'name', Cfg, []},
			      {'reference', Ref, []}]},
		      Env = [{aid, AID}, {user, User},
			     {'transaction-id', TID},
			     {yang, <<"exodm.yang">>}|Env0],
		      %% Must map and queue in different loops
		      _ = [exodm_db_config:map_device_to_cached_values(
			     AID, Cfg, Ref, DID) || DID <- Devices],
		      _ = [exodm_rpc_handler:queue_message(
			     Db, AID, to_device,
			     [{'device-id', DID}|Env], RPC) || DID <- Devices],
		      exodm_db_config:switch_to_pending(AID, Cfg),
		      {ok, result_code(ok)};
		  [] ->
		      %% Is this an error or ok?
		      {ok, result_code(ok)}
	      end
      end).

yang_data(<<"file:", Filename/binary>>, Env) ->
    case lists:keyfind(Filename, 1, proplists:get_value(files, Env, [])) of
	{_, Data} ->
	    Data;
	false ->
	    error(?OBJECT_NOT_FOUND)
    end;
yang_data(Y, _) ->
    Y.


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
	    result_code(?VALIDATION_FAILED);
	false ->
	    Res = exodm_db_yang:write(AID, N, Y),
	    create_yang_module1(Res)
    end.

create_yang_module1(ok) ->
    {ok, result_code(ok)};
create_yang_module1({error, {Line, Fmt, Arg } } )->
    ?info("~p:json_rpc(create-yang-module): Error: Line: ~p: ~p\n",
	  [?MODULE, Line, io_lib:fwrite(Fmt, Arg) ]),
    {ok, result_code(?VALIDATION_FAILED)};
create_yang_module1(Err) ->
    ?info("~p:json_rpc(create-yang-module): Error: ~p\n",
	  [?MODULE, Err ]),
    {ok, result_code(?VALIDATION_FAILED)}.

delete_yang_module(AID, Name) ->
    case exodm_db_yang:delete(AID, Name) of
        ok ->
            {ok, result_code(ok)};
	_Error -> %%?? more error codes internal error?
            {ok, result_code(?OBJECT_NOT_FOUND)}
    end.

lookup_yang_module(AID, Name) ->
    case exodm_db_yang:lookup(AID, Name) of 
        {ok, Src} ->
            {ok, result_code(ok) ++ 
                 [{'yang-modules', {array, [binary_to_list(Src)]}}]};
 	_Error -> %%?? more error codes internal error?
            ?debug("lookup failed, reason ~p", [_Error]),
            {ok, result_code(?OBJECT_NOT_FOUND)}
    end.
    
list_yang_modules(AID, N, Prev, ?ASC) ->
    Res = exodm_db_yang:list_next(AID, N, Prev),
    {ok, result_code(ok) ++ [{'yang-modules', {array, Res}}]};
list_yang_modules(AID, N, Prev, ?DESC) ->
    Res = exodm_db_yang:list_prev(AID, N, Prev),
    {ok, result_code(ok) ++ [{'yang-modules', {array, Res}}]}.
    
list_exec_permission(AID, ?EXODM, RName) ->
    case exodm_db_account:rpc_roles(AID, RName) of
        RList when is_list(RList) ->
            Res = [binary_to_atom(Role, latin1) ||  Role <- RList],
            {ok, result_code(ok) ++ [{'roles', {array, Res}}]};
        {error, Reason} ->
            {ok, result_code(Reason)}
    end;
list_exec_permission(_AID, _MName, _RName) ->
    %% Not implemented yet FIXME
    ?debug("not implemented yet", []),
    {ok, result_code('not-implemented-yet')}.
    

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
    Res = exodm_db_device_type:list_next(AID, N, Prev), 
    {ok, result_code(ok) ++
	[{'device-types', {array, [ {struct, DT} || DT <- Res]}}]}.

list_device_type_members(AID, Name, N, Prev) ->
    Res = exodm_db_device_type:list_devices(AID, Name, N, Prev),
    ?debug("device type members (~p) = ~p~n", [Name, Res]),
    {ok, result_code(ok) ++ [{'device-type-members', {array, Res}}]}.

create_device_group(AID, GName, URL) ->    
    ok = exodm_db_group:new(AID, [{name, GName},
				  {url, URL}]),
    {ok, result_code(ok)}.
update_device_group(AID, GID, Values) ->
    Values2 = lists:map(fun({'notification-url',U, _}) -> {url,U};
			   ({K,V,_}) -> {K,V}
			end, Values),
    case exodm_db_group:update(AID, GID, Values2) of
	ok -> {ok, result_code(ok)};
	{error, not_found} ->
	    {ok, result_code(?OBJECT_NOT_FOUND)}
    end. 

delete_device_group(AID, GID) ->   
    case exodm_db_group:delete(AID, GID) of
	ok -> {ok, result_code(ok)};
	{error, not_found} ->
	    {ok, result_code(?OBJECT_NOT_FOUND)}
    end.
    
list_device_groups(AID, N, Prev0, Tail) ->
    lager:debug("aid ~p",[AID]),
    Prev = exodm_db:encode_id(Prev0),
    {Tab,FullPrev,F} =
	case lists:keyfind('device-id', 1, Tail) of
	    {'device-id', DID, _} ->
		{exodm_db_device:table(AID),
		 kvdb_conf:join_key([DID, <<"groups">>, Prev]),
		 fun(Key) ->
			 Grp = lists:last(kvdb_conf:split_key(Key)),
			 exodm_db_group:lookup(AID, Grp)
		 end};
	    false ->
                lager:debug("no tail",[]),
		{exodm_db_group:table(AID),
		 Prev,
		 fun(Key) ->
			 [Grp] = kvdb_conf:split_key(Key),
			 exodm_db_group:lookup(AID, Grp)
		 end}
	end,
    lager:debug("tab ~p, FullPrev ~p",[Tab,FullPrev]),
    Res =
	exodm_db:in_transaction(
	  fun(_) ->
		  exodm_db:list_next(Tab, N, FullPrev, F)
	  end),
    ?debug("groups = ~p~n", [Res]),
    {ok, result_code(ok) ++ 
	[{'device-groups',
	   {array, [
		    {struct,
		     [{'group-id', G},
		      {'notification-url', U}]} ||
		       [{'group-id',G},{url,U}|_] <- Res]}}]}.

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
    {ok, result_code(ok) ++ [{'device-group-members', {array, Res}}]}.

provision_device(AID, I, Opts) -> 
    {ok, ?catch_result(exodm_db_device:new(AID, I, Opts))}.

lookup_device(AID, I) ->   
    Res = exodm_db_device:lookup(AID, I),
    case Res of
	[] ->
	    {ok, result_code(?DEVICE_NOT_FOUND)};
	[_|_] ->
	    {ok, result_code(ok) ++
		 [{'devices', {array, [{struct, Res} || Res =/= []]}}]}
    end.
 
-define(IS_CONNECTED, <<"is-connected">>).

lookup_device_attributes(AID, I, Attrs) ->   
   ?debug("lookup_device_attributes: ~p~n", [Attrs]),
    Res = exodm_db_device:lookup_attrs(AID, I, Attrs -- [?IS_CONNECTED]),
    case Res of
	[] ->
	    {ok, result_code(?DEVICE_NOT_FOUND)};
	[_|_] ->
	    State = device_state(AID, I, Attrs),
	    ?debug("lookup_device_attributes: result ~p~n", [State ++ Res]),
	    {ok, result_code(ok) ++ attrs2yang(State ++ Res)}
    end.


device_state(AID, I, Attrs) ->
    case lists:member(?IS_CONNECTED, Attrs) of
	true ->[{?IS_CONNECTED, is_connected(AID, I)}];
	false -> []
    end.

is_connected(AID, I) ->
    ?debug("is_connected: ~p~n", [I]),
    case exodm_rpc_handler:device_sessions(AID, I) of
	[] -> <<"false">>;
	 _S -> <<"true">>
    end.
	     
attrs2yang(Res) ->
 [{'attributes',{array, [{struct, [{'name', Name},
				   {'val', Value}]} || 
			    {Name, Value} <-  Res, Res =/= []]}}].
 
update_device(AID, I, Opts) ->   
    {ok, ?catch_result(exodm_db_device:update(AID, I, kvl(Opts)))}.
   
deprovision_devices(AID, DevIdList)  -> 
    exodm_db_device:delete_devices(AID, DevIdList),
    {ok, result_code(ok)}.
    %% later
    %% {ok, ?catch_result(exodm_db_device:delete_devices(AID, DevIdList))}.
    
list_devices(AID, N, Prev) ->
    Res =
	exodm_db:in_transaction(
	  fun(_) ->
		  exodm_db_device:list_next(AID, N, Prev)
	  end),
    ?debug("devices = ~p~n", [Res]),
    {ok, result_code(ok) ++ 
	[{'devices', {array, [ {struct, D} || D <- Res ]} }]}.  
  
list_devices_attributes(AID, N, Prev, Attrs, _Pattern, ?ASC) ->
    ?debug("list devices ascending ~n", []),
    %% Pattern ignored so far
    DeviceIds = [Id || [{'device-id', Id} | _Tail] <- 
			   exodm_db_device:list_next(AID, N, Prev)], 
    
    list_devices_attributes_cont(AID, DeviceIds, Attrs);	
list_devices_attributes(AID, N, Next, Attrs, _Pattern, ?DESC) ->
    ?debug("list devices descending ~n", []),
    %% Pattern ignored so far
    DeviceIds = [Id || [{'device-id', Id} | _Tail] <- 
			   exodm_db_device:list_prev(AID, N, Next)],
    list_devices_attributes_cont(AID, DeviceIds, Attrs).

list_devices_attributes_cont(AID, DeviceIds, Attrs) ->
    ?debug("devices = ~p~n", [DeviceIds]),
    {ok, result_code(ok) ++ 
	 [{'devices', 
	   {array, 
	    [{struct, device_attrs(AID, D, Attrs)} || D <- DeviceIds ]}}]}.

device_attrs(AID, Id, Attrs) ->
    As = exodm_db_device:lookup_attrs(AID, Id, Attrs -- [?IS_CONNECTED]),
    State = device_state(AID, Id, Attrs),
    ?debug("devices_attrs: ~p: ~p~n",  [Id, State ++ As]),
    [{'device-id', Id} | attrs2yang(State ++ As)].
    
%% 
%% Check if current user is using ssl / from localhost
%% 
has_root_env(Env) ->
    proplists:get_bool(ssl, Env) 
		andalso
		  ((proplists:get_value(ip, Env) =:= {127,0,0,1})
		   orelse
		     (proplists:get_value(ip,Env) =:= {0,0,0,0,0,0,0,1})).

current_user_account() ->
    exodm_db_user:list_accounts(exodm_db_session:get_user()).

is_root(Env) when is_list(Env)->
    is_root(proplists:get_value(user, Env)); 
is_root(?EXODM_ADMIN) -> true;
is_root(root) -> true;
is_root(_Other) -> ?debug("user ~p", [_Other]), false.

    
%% to_uint32(<<I:32>>) -> I;
%% to_uint32(I) when is_integer(I) -> I.

kvl([{K,V}  |Opts]) -> [{K,V}|kvl(Opts)];
kvl([{K,V,_}|Opts]) -> [{K,V}|kvl(Opts)];
kvl([]) -> [].

