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

-define(EXODM, <<"exodm">>). 
-define(EXODM_ADMIN, <<"exodm_admin">>). 

-define(catch_result(Expr),
	result_code(try Expr
		    catch
			error:'permission-denied' = E -> E;
			error:'validation-failed' = E -> E;
			error:'object-exists'     = E -> E;
			error:'device-not-found'  = E -> E;
			error:'object-not-found'  = E -> E;
			error:'object-not-empty'  = E -> E;
                        error:'account-not-specified' = E -> E
		    end)).

%%--------------------------------------------------------------------
%% @doc
%% Checks if User is the system admin.
%%
%% @end
%%--------------------------------------------------------------------
-spec exodm_admin(UID::binary()) -> boolean().

exodm_admin(<<"exodm_admin">>) -> true;
exodm_admin(_Other) -> false.
    

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
    [{result, <<"device-not-found">>}];

result_code('account-not-specified') ->
    [{result, <<"account-not-specified">>}].


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

json_rpc(RPC, Env) ->
    ?debug("~p:json_rpc(~p, ~p)~n", [?MODULE, RPC, Env]),
    json_rpc_(RPC, Env).


%% Account
%% Authenticated by super user 'exodm' (only allowed using SSL?)
%% create-account
json_rpc_({call, ?EXODM, <<"create-account">>,
	   [{'name', Name, _Type1},
	    {'admin-user',[Admin],_Type2}| _]} = _RPC, Env) ->
    case has_root_access(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = create_account(Name, Admin),
	    exodm_db_session:unset_trusted_proc(),
	    Res;
	false ->
	    {ok, result_code('permission-denied')}
    end;

json_rpc_({call, ?EXODM, <<"update-account">>,
	   [{'name', _Name, _}|_Opts]} = _RPC, Env) ->
    case has_root_access(Env) of
	true ->
	    %% Not implemented ???
	    {ok, result_code(ok)};
	false ->
	    {ok, result_code('permission-denied')}
    end;

json_rpc_({call, ?EXODM, <<"delete-account">>,
	   [{'name', Name, _}|_Opts]} = _RPC, Env) ->
    case has_root_access(Env) andalso (Name =/= <<"exodm">>) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = delete_account(Name),
	    exodm_db_session:unset_trusted_proc(),
	    Res;
	false ->
	    {ok, result_code('permission-denied')}
    end;

%% User
json_rpc_({call, ?EXODM, <<"list-accounts">>,
	   [{'n', N, _},
	    {'previous', Prev, _}]} = _RPC, Env) ->
    case has_root_access(Env) of
	true -> list_accounts(N, Prev);
	false -> {ok, result_code('permission-denied')}
    end;

json_rpc_({call, ?EXODM, <<"create-user">>,
	   [{'uname', Name, _Type1} | Options]} , Env) ->
    case has_root_access(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = create_user(Name, Options),
	    exodm_db_session:unset_trusted_proc(),
	    Res;
	false ->
	    {ok, result_code('permission-denied')}
    end;
json_rpc_({call, ?EXODM, <<"delete-user">>,
	   [{'uname', Name, _}|_Opts]} = _RPC, Env) ->
    case has_root_access(Env) andalso (Name =/= <<"exodm-admin">>) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = delete_user(Name),
	    exodm_db_session:unset_trusted_proc(),
	    Res;
	false ->
	    {ok, result_code('permission-denied')}
    end;

json_rpc_({call, ?EXODM, <<"list-users">>,
	   [{'account', Account, _},
	    {'n', N, _},
	    {'previous', Prev, _}]} = _RPC, _Env) ->
    list_users(Account, N, Prev);

json_rpc_({call, ?EXODM, <<"list-users">>,
	   [{'n', N, _},
	    {'previous', Prev, _}]} = _RPC, _Env) ->
    list_users(N, Prev);

json_rpc_({call, ?EXODM, <<"add-users-to-account">>,
	   [{'account', Account, _},
	    {'role', Role, _},
	    {'unames', UNames, _}]}, Env) ->
    {ok, ?catch_result(exodm_db_account:add_users(Account, Role, UNames,
						 has_root_access(Env)))};
json_rpc_({call, ?EXODM, <<"remove-users-from-account">>,
	   [{'account', Account, _},
	    {'role', Role, _},
	    {'unames', UNames, _}]}, Env) ->
    {ok, ?catch_result(exodm_db_account:remove_users(Account, Role, UNames,
						 has_root_access(Env)))};
json_rpc_({call, ?EXODM, <<"list-account-users">>,
	   [{'n', N, _},
	    {'previous', Prev, _}]} = _RPC, _Env) ->
    case current_user_account() of
        [AID] -> list_users(AID, N, Prev);    
        _Other -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"list-account-users">>,
	   [{'account', Account, _},
	    {'n', N, _},
	    {'previous', Prev, _}]} = _RPC, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> list_users(AID, N, Prev);
	[] -> {ok, result_code('object-not-found')}
    end;


%% Config set
json_rpc_({call, ?EXODM, <<"create-config-set">>,
	   [{'account', Account, _},
	    {'name', _N, _},
	    {yang, _Y, _} | _Rest] = Attrs} = _RPC, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> create_config_set(AID, Attrs);
	[] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, ?EXODM, <<"create-config-set">>,
	   [{'name', _N, _},
	    {yang, _Y, _} | _Rest] = Attrs} = _RPC, _Env) ->
    case current_user_account() of
        [AID] -> create_config_set(AID, Attrs);
        _Other -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"update-config-set">>,
	   [{'account', Account, _},
	    {'name', Name, _}|Attrs]} = _RPC, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> update_config_set(AID, Name, Attrs);
	[] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, ?EXODM, <<"update-config-set">>,
	   [{'name', Name, _}|Attrs]} = _RPC, _Env) ->
    case current_user_account() of
        [AID] -> update_config_set(AID, Name, Attrs);
        _Other -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"delete-config-set">>,
	   [{'account', Account, _},
	    {'name', Name, _}]} = _RPC, _Env) ->
     case exodm_db_account:lookup_by_name(Account) of
	[AID] -> delete_config_set(AID, Name);
	[] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, ?EXODM, <<"delete-config-set">>,
	   [{'name', Name, _}]} = _RPC, _Env) ->
    case current_user_account() of
        [AID] -> delete_config_set(AID, Name);
        _Other -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"list-config-sets">>,
	   [{'account', Account, _},
	    {n, N, _}, 
	    {previous, Prev, _}| Tail] = _Cfg} = _RPC, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> list_config_sets(AID, N, Prev, Tail);
	[] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, ?EXODM, <<"list-config-sets">>,
	   [{n, N, _}, 
	    {previous, Prev, _}|Tail] = _Cfg} = _RPC, _Env) ->
    case current_user_account() of
        [AID] -> list_config_sets(AID, N, Prev, Tail);
        _Other -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"add-config-set-members">>,
	   [{'account', Account, _},
	    {'name', CfgDataList, _},
	    {'dev-id', DevIdList, _}]} = _RPC, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> add_config_set_members(AID, CfgDataList, DevIdList);
	[] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, ?EXODM, <<"add-config-set-members">>,
	   [{'name', CfgDataList, _},
	    {'dev-id', DevIdList, _}]} = _RPC, _Env) ->
    case current_user_account() of
        [AID] -> add_config_set_members(AID, CfgDataList, DevIdList);
        _Other -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"remove-config-set-members">>,
	   [{'account', Account, _},
	    {'name', Names, _},
	    {'dev-id', DIDs, _}]} = _RPC, _Env) ->
    ?debug("~p:json_rpc(remove-config-set-members) dev-id:~p names:~p~n",
           [ ?MODULE, DIDs, Names ]),
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> remove_config_set_members(AID, Names, DIDs);
	[] -> {ok, result_code('object-not-found')}
    end;
    

json_rpc_({call, ?EXODM, <<"remove-config-set-members">>,
	   [{'name', Names, _},
	    {'dev-id', DIDs, _}]} = _RPC, _Env) ->
    ?debug("~p:json_rpc(remove-config-set-members) dev-id:~p names:~p~n",
           [ ?MODULE, DIDs, Names ]),
    case current_user_account() of
        [AID] -> remove_config_set_members(AID, Names, DIDs);
        _Other -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"list-config-set-members">>,
	   [{'account', Account, _},
	    {'name', Name, _}, 
	    {'n', N, _}, 
	    {'previous', Prev, _}] = _Params},
	  _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> list_config_set_members(AID, Name, N, Prev);
	[] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, ?EXODM, <<"list-config-set-members">>,
	   [{'name', Name, _}, 
	    {'n', N, _}, 
	    {'previous', Prev, _}] = _Params},
	  _Env) ->
    case current_user_account() of
        [AID] -> list_config_set_members(AID, Name, N, Prev);
        _Other -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"push-config-set">>,
	   [{'account', Account, _},
	    {'name', Cfg, _}]} = _RPC, Env) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> push_config_set(AID, Cfg, Env);
	[] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, ?EXODM, <<"push-config-set">>,
	   [{'name', Cfg, _}]} = _RPC, Env) ->
    case current_user_account() of
        [AID] -> push_config_set(AID, Cfg, Env);
        _Other -> {ok, result_code('account-not-specified')}
    end;
    

%% Yang module
%% User repo
json_rpc_({call, ?EXODM, <<"create-yang-module">>,
	   [{'account', Account, _},
	    {repository, R, _},
	    {name, N, _},
	    {'yang-module', Y, _}]} = _RPC, _Env) 
  when R =:= ?USER_REPOSITORY ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> create_yang_module(AID, N, Y);
	[] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, ?EXODM, <<"create-yang-module">>,
	   [{repository, R, _},
	    {name, N, _},
	    {'yang-module', Y, _}]} = _RPC, _Env) 
  when R =:= ?USER_REPOSITORY ->
    case current_user_account() of
        [AID] -> create_yang_module(AID, N, Y);
        _Other -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"delete-yang-module">>,
	   [{'account', Account, _},
	    {repository, R, _},
	    {name, Name, _}]} = _RPC, _Env) 
  when R =:= ?USER_REPOSITORY ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> delete_yang_module(AID, Name);
	[] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, ?EXODM, <<"delete-yang-module">>,
	   [{repository, R, _},
	    {name, Name, _}]} = _RPC, _Env) 
  when R =:= ?USER_REPOSITORY ->
    case current_user_account() of
        [AID] -> delete_yang_module(AID, Name);
        _Other -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"list-yang-modules">>,
	   [{'account', Account, _},
	    {repository, R, _},
	    {n,N,_},
	    {previous, Prev, _}]} = _RPC, _Env) 
  when R =:= ?USER_REPOSITORY ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> list_yang_modules(AID, N, Prev);
	[] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, ?EXODM, <<"list-yang-modules">>,
	   [{repository, R, _},
	    {n,N,_},
	    {previous, Prev, _}]} = _RPC, _Env) 
  when R =:= ?USER_REPOSITORY ->
    case current_user_account() of
        [AID] -> list_yang_modules(AID, N, Prev);
        _Other -> {ok, result_code('account-not-specified')}
    end;

%% Yang module
%% System repo
json_rpc_({call, ?EXODM, <<"create-yang-module">>,
	   [{'account', "exodm", _},
	    {repository, R, _},
	    {name, N, _},
	    {'yang-module', Y, _}]} = _RPC, Env) 
  when R =:= ?SYSTEM_REPOSITORY ->
    case has_root_access(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = create_yang_module(system, N, Y),
	    exodm_db_session:unset_trusted_proc(),
	    Res;
	false ->
	    {ok,result_code('permission-denied')}
    end;

json_rpc_({call, ?EXODM, <<"create-yang-module">>,
	   [{repository, R, _},
	    {name, N, _},
	    {'yang-module', Y, _}]} = _RPC, Env) 
  when R =:= ?SYSTEM_REPOSITORY ->
    case has_root_access(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = create_yang_module(system, N, Y),
	    exodm_db_session:unset_trusted_proc(),
	    Res;
	false ->
	    {ok,result_code('permission-denied')}
    end;

json_rpc_({call, ?EXODM, <<"delete-yang-module">>,
	   [{'account', "exodm", _},
	    {repository, R, _},
	    {name, Name, _}]} = _RPC, Env) 
  when R =:= ?SYSTEM_REPOSITORY ->
    case has_root_access(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = delete_yang_module(system, Name),
	    exodm_db_session:unset_trusted_proc(),
	    Res;
	false ->
	    {ok,result_code('permission-denied')}
    end;
json_rpc_({call, ?EXODM, <<"delete-yang-module">>,
	   [{repository, R, _},
	    {name, Name, _}]} = _RPC, Env) 
  when R =:= ?SYSTEM_REPOSITORY ->
    case has_root_access(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = delete_yang_module(system, Name),
	    exodm_db_session:unset_trusted_proc(),
	    Res;
	false ->
	    {ok,result_code('permission-denied')}
    end;


json_rpc_({call, ?EXODM, <<"list-yang-modules">>,
	   [{'account', "exodm", _},
	    {repository, R, _},
	    {n, N, _},
	    {previous, Prev, _}]} = _RPC, Env) 
  when R =:= ?SYSTEM_REPOSITORY ->
    case has_root_access(Env) of
	true ->
	    exodm_db_session:set_trusted_proc(),
	    Res = list_yang_modules(system, N, Prev),
	    exodm_db_session:unset_trusted_proc(),
	    Res;
	false ->
	    {ok,result_code('permission-denied')}
    end;

json_rpc_({call, ?EXODM, <<"list-yang-modules">>,
	   [{repository, R, _},
	    {n, N, _},
	    {previous, Prev, _}]} = _RPC, Env) 
  when R =:= ?SYSTEM_REPOSITORY ->
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
json_rpc_({call, ?EXODM, <<"create-device-type">>,
	   [{'account', Account, _},
	    {'name', Name, _}|Opts] = _Cfg}, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
		      [AID] -> create_device_type(AID, Name, Opts);
		      [] -> {ok, result_code('object-not-found')}
    end;  
	
json_rpc_({call, ?EXODM, <<"create-device-type">>,
	   [{'name', Name, _}|Opts] = _Cfg}, _Env) ->
    case current_user_account() of
        [AID] -> create_device_type(AID, Name, Opts);
        _Other -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"update-device-type">>,
	   [{'account', Account, _},
	    {'name', Name, _}|Opts] = _Cfg}, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
		      [AID] -> update_device_type(AID, Name, Opts);
		      [] -> {ok, result_code('object-not-found')}
    end;  

json_rpc_({call, ?EXODM, <<"update-device-type">>,
	   [{'name', Name, _}|Opts] = _Cfg}, _Env) ->
    case current_user_account() of
        [AID] -> update_device_type(AID, Name, Opts);
        _Other -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"delete-device-type">>,
	   [{'account', Account, _},
	    {'name', Name, _}]}, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
		      [AID] -> delete_device_type(AID, Name);
		      [] -> {ok, result_code('object-not-found')}
    end;  

json_rpc_({call, ?EXODM, <<"delete-device-type">>,
	   [{'name', Name, _}]}, _Env) ->
    case current_user_account() of
        [AID] -> delete_device_type(AID, Name);
        _Other -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"list-device-types">>,
	   [{'account', Account, _},
	    {'n', N, _},
	    {'previous', Prev, _}]} = _RPC, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
		      [AID] -> list_device_types(AID, N, Prev);
		      [] -> {ok, result_code('object-not-found')}
    end;  

json_rpc_({call, ?EXODM, <<"list-device-types">>,
	   [{'n', N, _},
	    {'previous', Prev, _}]} = _RPC, _Env) ->
    case current_user_account() of
        [AID] -> list_device_types(AID, N, Prev);
        _Other -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"list-device-type-members">>,
	   [{'account', Account, _},
	    {'name', Name, _}, 
	    {'n', N, _}, 
	    {'previous', Prev, _}] = _Params},
	  _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
        [AID] -> list_device_type_members(AID, Name, N, Prev);
        [] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, ?EXODM, <<"list-device-type-members">>,
	   [{'name', Name, _}, 
	    {'n', N, _}, 
	    {'previous', Prev, _}] = _Params},
	  _Env) ->
    case current_user_account() of
        [AID] -> list_device_type_members(AID, Name, N, Prev);
        _Other -> {ok, result_code('account-not-specified')}
    end;

%% Device group
json_rpc_({call, ?EXODM, <<"create-device-group">>,
	   [{'account', Account, _},
	    {'name', GName, _},
	    {'notification-url', URL, _}] = _Cfg} = _RPC, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
		      [AID] -> create_device_group(AID, GName, URL);
		      [] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, ?EXODM, <<"create-device-group">>,
	   [{'name', GName, _},
	    {'notification-url', URL, _}] = _Cfg} = _RPC, _Env) ->
    case current_user_account() of
        [AID] -> create_device_group(AID, GName, URL);
        _Other -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"update-device-group">>,
	   [{'account', Account, _},
	    {'gid', GID, _}|Values] = _Cfg} = _RPC, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> update_device_group(AID, GID, Values);
	[] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, ?EXODM, <<"update-device-group">>,
	   [{'gid', GID, _}|Values] = _Cfg} = _RPC, _Env) ->
    case current_user_account() of
        [AID] -> update_device_group(AID, GID, Values);
        _Other -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"delete-device-group">>,
	   [{'account', Account, _},
	    {'gid', GID, _}] = _Cfg} = _RPC, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> delete_device_group(AID, GID);
	[] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, ?EXODM, <<"delete-device-group">>,
	   [{'gid', GID, _}] = _Cfg} = _RPC, _Env) ->
    case current_user_account() of
        [AID] -> delete_device_group(AID, GID);
        _Other -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"list-device-groups">>,
	   [{'account', Account, _},
	    {n, N, _},
	    {previous, Prev, _}|Tail] = _Cfg} = _RPC, _Env) ->
    lager:debug("list-device-groups: account ~p", [Account]),
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> list_device_groups(AID, N, Prev, Tail);
	[] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, ?EXODM, <<"list-device-groups">>,
	   [{n, N, _},
	    {previous, Prev, _}|Tail] = _Cfg} = _RPC, _Env) ->
    case current_user_account() of
        [AID] -> list_device_groups(AID, N, Prev, Tail);
        _Other -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"add-device-group-members">>,
	   [{'account', Account, _},
	    {'device-groups', GIDs, _},
	    {'dev-id', DIDs, _}] = _Cfg}, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> add_device_group_members(AID, GIDs, DIDs);
	[] -> {ok, result_code('object-not-found')}
    end;
    
json_rpc_({call, ?EXODM, <<"add-device-group-members">>,
	   [{'device-groups', GIDs, _},
	    {'dev-id', DIDs, _}] = _Cfg}, _Env) ->
    case current_user_account() of
        [AID] -> add_device_group_members(AID, GIDs, DIDs);
        _Other -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"remove-device-group-members">>,
	   [{'account', Account, _},
	    {'device-groups', GIDs, _},
	    {'dev-id', DIDs, _}] = _Cfg}, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> remove_device_group_members(AID, GIDs, DIDs);
	[] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, ?EXODM, <<"remove-device-group-members">>,
	   [{'device-groups', GIDs, _},
	    {'dev-id', DIDs, _}] = _Cfg}, _Env) ->
    case current_user_account() of
        [AID] -> remove_device_group_members(AID, GIDs, DIDs);
        [] -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"list-device-group-members">>,
	   [{'account', Account, _},
	    {'gid', GID, _}, 
	    {'n', N, _}, 
	    {'previous', Prev, _}] = _Params}, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> list_device_group_members(AID, GID, N, Prev);
	[] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, ?EXODM, <<"list-device-group-members">>,
	   [{'gid', GID, _}, 
	    {'n', N, _}, 
	    {'previous', Prev, _}] = _Params}, _Env) ->
    case current_user_account() of
        [AID] -> list_device_group_members(AID, GID, N, Prev);
        [] -> {ok, result_code('account-not-specified')}
    end;

%% Device
json_rpc_({call, _, <<"provision-device">>,
	   [{'account', Account, _},
	    {'dev-id', I, _},
	    {'device-type', T, _} |
	    Opts]} = _RPC, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> provision_device(AID, I, [{'device-type', T}|kvl(Opts)]);
	[] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, _, <<"provision-device">>,
	   [{'dev-id', I, _},
	    {'device-type', T, _} |
	    Opts]} = _RPC, _Env) ->
    case current_user_account() of
        [AID] -> provision_device(AID, I, [{'device-type', T}|kvl(Opts)]);
        [] -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, _, <<"provision-device">>,
	   [{'account', Account, _},
	    {DevId, DID, _}|Opts] = _Cfg} = _RPC, _Env)
  when DevId=='dev-id'; DevId=='device-id' ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> provision_device(AID, DID, kvl(Opts));
	[] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, _, <<"provision-device">>,
	   [{DevId, DID, _}|Opts] = _Cfg} = _RPC, _Env)
  when DevId=='dev-id'; DevId=='device-id' ->
    case current_user_account() of
        [AID] -> provision_device(AID, DID, kvl(Opts));
        [] -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, _, <<"lookup-device">>,
	   [{'account', Account, _},
	    {'dev-id', I, _}]}, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> lookup_device(AID, I);
	[] -> {ok, result_code('object-not-found')}
    end;
    
json_rpc_({call, _, <<"lookup-device">>,
	   [{'dev-id', I, _}]}, _Env) ->
    case current_user_account() of
        [AID] -> lookup_device(AID, I);
        [] -> {ok, result_code('account-not-specified')}
    end;
    
json_rpc_({call, _, <<"update-device">>,
	   [{'account', Account, _},
	    {'dev-id', I, _} | Opts]}, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> update_device(AID, I, Opts);
	[] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, _, <<"update-device">>,
	   [{'dev-id', I, _} | Opts]}, _Env) ->
    case current_user_account() of
        [AID] -> update_device(AID, I, Opts);
        [] -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, _, <<"deprovision-devices">>,
	   [{'account', Account, _},
	    {'dev-id', DevIdList, _}]}, _Env) ->
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> deprovision_devices(AID, DevIdList);
	[] -> result_code('object-nsot-found')
    end;

json_rpc_({call, _, <<"deprovision-devices">>,
	   [{'dev-id', DevIdList, _}]}, _Env) ->
    case current_user_account() of
        [AID] -> deprovision_devices(AID, DevIdList);
        [] -> {ok, result_code('account-not-specified')}
    end;

json_rpc_({call, ?EXODM, <<"list-devices">>,
	   [{'account', Account, _},
	    {n, N, _}, 
	    {previous, Prev, _}] = _Cfg} = _RPC, _Env) ->
    lager:debug("list-devices: account ~p", [Account]),
    case exodm_db_account:lookup_by_name(Account) of
	[AID] -> list_devices(AID, N, Prev);
	[] -> {ok, result_code('object-not-found')}
    end;

json_rpc_({call, ?EXODM, <<"list-devices">>,
	   [{n, N, _}, 
	    {previous, Prev, _}] = _Cfg} = _RPC, _Env) ->
    case current_user_account() of
        [AID] -> list_devices(AID, N, Prev);
        [] -> {ok, result_code('account-not-specified')}
    end;

%% Failure case
json_rpc_(RPC, _ENV) ->
    ?info("~p:json_rpc_() Unknown RPC: ~p\n", [ ?MODULE, RPC ]),
    {ok, result_code('validation-failed')}.

%%--------------------------------------------------------------------
%%
%% Internal functions
%%
%%--------------------------------------------------------------------
create_account(Name, Admin) ->
    {ok, ?catch_result(exodm_db_account:new([{name,Name},{admin, Admin}]))}.
    
delete_account(Name) ->
    {ok, ?catch_result(exodm_db_account:delete(Name))}.
    
list_accounts(N, Prev) ->
    Res = lists:map(fun([{<<"id">>,_},{<<"name">>,Name}]) -> Name end,
		    exodm_db_account:list_accounts(N, Prev)),
    {ok, [{'accounts', {array,Res}}]}.
    
create_user(Name, Options) ->
    %% Remove type info from opts
    Opts = [{Opt, Value} || {Opt, Value, _Type} <- Options],
    {ok, ?catch_result(exodm_db_user:new(Name, Opts))}.
    
delete_user(Name) ->
    {ok, ?catch_result(exodm_db_user:delete(Name))}.
   
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
    lager:debug("aid ~p",[AID]),
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
    {ok, ?catch_result(exodm_db_device:new(AID, I, Opts))}.

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

current_user_account() ->
    exodm_db_user:list_accounts(exodm_db_session:get_user()).
    
to_uint32(<<I:32>>) -> I;
to_uint32(I) when is_integer(I) -> I.

kvl([{K,V}  |Opts]) -> [{K,V}|kvl(Opts)];
kvl([{K,V,_}|Opts]) -> [{K,V}|kvl(Opts)];
kvl([]) -> [].

