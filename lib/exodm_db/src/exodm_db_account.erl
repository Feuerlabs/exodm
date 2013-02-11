%%% @author Ulf Wiger <ulf@feuerlabs.com>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%     Exosense account db
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_account).

-include_lib("lager/include/log.hrl").
-include_lib("exodm/include/exodm.hrl").
-include("exodm_db.hrl").

%% Gproc 
-export([subscribe/1,
	 unsubscribe/1]).
%% Startup
-export([create_exodm_account/0]).
-export([table/0]).
-export([init/0]).

%% Account handling
-export([new/1,
	 delete/1, 
	 update/2, 
	 lookup/1, 
	 lookup_by_name/1, 
	 exist/1,
	 is_empty/1]).
-export([list_accounts/2,
	 list_account_keys/0,
	 list_users/3,
	 list_admins/3,
	 list_roles/3,
	 list_user_roles/2,
	 list_groups/1,
	 list_groups/2,
	 system_specs/1]).
-export([add_users/4,
	 remove_users/4,
	 user_exists/2]).
-export([create_role/3,
	 role_def/2,
	 role_exists/2,
         rpc_role_list/0,
         rpc_roles/2,
         rpc_permission/2]).
-export([register_protocol/2, 
	 is_protocol_registered/2]).
-export([incr_request_id/1,
	 incr_transaction_id/1]).

%%-export([fold_accounts/2]).
%%-export([list_users/1, list_users/2]).
%%-export([list_admins/1]).

-import(exodm_db, [binary_opt/2, to_binary/1]).

%% Table owned by this module
-define(TAB, <<"acct">>).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("../test/feuerlabs_eunit.hrl").
-endif.

%%--------------------------------------------------------------------
%% @doc
%%  This modules table.
%% @end
%%--------------------------------------------------------------------
table() ->
    ?TAB.

%%--------------------------------------------------------------------
%% @doc
%%  Init this modules table.
%% @end
%%--------------------------------------------------------------------
init() ->
    exodm_db:in_transaction(
      fun(_) ->
	      exodm_db:add_table(?TAB, [name])
      end).


%% Gproc pub/sub ----------------------
subscribe(Event) when Event==add; Event==delete ->
    gproc:reg({p, l, {?MODULE, Event}}).

unsubscribe(Event) when Event==add; Event==delete ->
    catch gproc:unreg({p, l, {?MODULE, Event}}),
    ok.

publish(Event, Data) when Event==add; Event==delete ->
    gproc:send({p, l, {?MODULE, Event}}, {?MODULE, Event, Data}).
%% END Gproc pub/sub ------------------


%%--------------------------------------------------------------------
%% Start up
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%%
%% Setup exodm account if needed
%% called from exodm_db:init - after other init!
%%
%% @end
%%--------------------------------------------------------------------
-spec create_exodm_account() -> ok.

create_exodm_account() ->
    exodm_db:in_transaction(
      fun(Db) ->
	      case lookup_by_name(<<"exodm">>) of
		  false ->
		      create_exodm_account_(Db);
		  _ ->
		      ok
	      end
      end).

create_exodm_account_(_Db) ->
    exodm_db_session:set_trusted_proc(),
    R=
	new(
	  [
	   {?ACC_OPT_NAME, <<"exodm">>},
	   {?ACC_OPT_ROOT, true},
	   {?ACC_OPT_ADMIN, 
	    [
	     {?ACC_OPT_UNAME, <<"exodm-admin">>},
	     {?ACC_OPT_FNAME, <<"Administrator">>},
	     {?ACC_OPT_PWD, atom_to_binary(erlang:get_cookie(),latin1)}
	    ]}]),
    exodm_db_session:unset_trusted_proc(),
    R.
    

%%--------------------------------------------------------------------
%% Account handling
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% Create new account.
%%
%% /<aid>/name  = <AccountName>
%% /<aid>/admin = <UID>
%%
%% Can throw error no_admin_user
%%
%% FIXME option validation
%% @end
%%--------------------------------------------------------------------
-spec new(list({Option::atom(), Value::term()})) -> 
		 {ok, AIDVal::binary()}.

new(Options) when is_list(Options) ->
    exodm_db:in_transaction(
      fun(_) ->
              case lookup_by_name(binary_opt(?ACC_OPT_NAME, Options)) of
		  false ->
                      {ok, AIDVal} = new_(Options),
                      publish(add, AIDVal),
                      ok;
                  _Other -> error('object-exists')
              end
      end).

new_(Options) ->
    %% initial check: there must be an admin
    UserOpts = case lists:keyfind(?ACC_OPT_ADMIN, 1, Options) of
		   false ->
		       error(no_admin_user);
		   {_, Os} when is_list(Os) ->
		       Os
	       end,
    Root = 
	case exodm_db_session:is_trusted_proc() of
	    true -> proplists:get_bool(?ACC_OPT_ROOT, Options);
	    false -> false
	end,
    AID = exodm_db_system:new_aid(),
    Key = exodm_db:escape_key(AID),
    exodm_db_group:init(AID),
    exodm_db_device_type:init(AID),
    insert(Key, ?ACC_DB_LAST_REQ, <<0:32>>),
    insert(Key, ?ACC_DB_LAST_TID, <<0:32>>),
    AdminUName = binary_opt(?ACC_OPT_UNAME, UserOpts),
    AcctName = case binary_opt(?ACC_OPT_NAME, Options) of
		   <<>> -> AdminUName;
		   Other -> Other
	       end,
    insert(Key, ?ACC_DB_NAME, AcctName),
    exodm_db_user:new(AdminUName, UserOpts),
    create_roles(AID, Root),
    add_admin_user(AID, AdminUName, Root),
    exodm_db_yang:init(AID),
    exodm_db_device:init(AID),
    exodm_db_config:init(AID),
    AIDVal = exodm_db:account_id_value(AID),

    %% lists:foreach(fun({I,Al}) ->
    %% 			  exodm_db:insert_alias(?TAB, Key, I, Al)
    %% 		  end, proplists:get_all_values(alias, Options)),
    %% add_user(AID, AdminUName, _RID = 1),  % must come after adding the user
    {ok, AIDVal}.

%%--------------------------------------------------------------------
%% @doc
%% Delete the account, caller must make sure that data
%% access via this account is first deleted.
%% @end
%%--------------------------------------------------------------------
-spec delete(Name::binary()) ->
		    ok.

delete(Name) when is_binary(Name) ->
    exodm_db:in_transaction(
      fun(_) ->
              case lookup_by_name(Name) of
		  AID when is_binary(AID) ->
                      case is_empty(AID) of
                          true -> 
                              ok = delete_(AID),
                              publish(delete, exodm_db:account_id_value(AID)),
                              ok;
                          false -> error('object-not-empty')
                      end;
                  false -> error('object-not-found')
              end
      end).

delete_(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:in_transaction(
      fun(_Db) ->
	      [Admin] = list_admins(AID, 2, <<"">>),
	      exodm_db_user:delete(Admin),
	      kvdb_conf:delete_tree(table(), AID),
	      publish(delete, exodm_db:account_id_value(AID)),
	      ok
      end).

%%--------------------------------------------------------------------
%% @doc
%% Update account.
%%
%% FIXME validate every item BEFORE insert!
%% @end
%%--------------------------------------------------------------------
-spec update(AID::binary(), 
	     list({Option::atom(), Value::term()})) ->
		    ok.

update(AID, Options) when is_binary(AID), is_list(Options) ->
    exodm_db:in_transaction(
      fun(_) ->
	      t_update(AID, Options)
      end).

t_update(AID0, Options) ->
    AID = check_access(AID0),
    update_(AID, Options).

update_(Key, Options) ->
    F =	fun({?ACC_OPT_NAME,Value}) ->
		NewName = to_binary(Value),
		insert(Key, ?ACC_DB_NAME, NewName)
	end,
    lists:foreach(F, Options).

%%--------------------------------------------------------------------
%% @doc
%% Lookup account
%%
%% @end
%%--------------------------------------------------------------------
%% Hmm, don't know how to specify ??
%%-spec lookup(AID::binary()) ->
%%		    list({?ACC_DB_ID, Aid::binary()},
%%			 {(?ACC_DB_NAME), Name::binary()})).
lookup(AID0) when is_binary(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    lookup_(AID).

lookup_(Key) ->
    <<_,ID/binary>> = lists:last(exodm_db:split_key(Key)),
    [{?ACC_DB_ID,ID}] ++
	read(Key, ?ACC_DB_NAME) ++
	read(Key, ?ACC_DB_ADMINS).

%%--------------------------------------------------------------------
%% @doc
%% Lookup account using account name.
%%
%% @end
%%--------------------------------------------------------------------
-spec lookup_by_name(AName::binary()) ->
			    AID::binary() | false.

lookup_by_name(AName) when is_binary(AName) ->
    case kvdb:index_get(kvdb_conf, ?TAB, name, AName) of
        [{Key, _, _}] ->
            %% e.g. [{<<"a00000001*name">>,[],<<"getaround">>}]
            [ID, _] = exodm_db:split_key(Key),
            ID;
        [] -> false
    end.

%%--------------------------------------------------------------------
%% @doc
%% Check if account exists
%%
%% @end
%%--------------------------------------------------------------------
-spec exist(AID0::binary()) ->
		   Exists::boolean().

exist(AID0) when is_binary(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    case read(AID, ?ACC_DB_NAME) of
	[] -> false;
	[_] -> true
    end.

%%--------------------------------------------------------------------
%% @doc
%% Check if account is empty
%% no device must exist
%% no yang specs must exist
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec is_empty(AID::binary()) ->
		      Empty::boolean().

is_empty(AID0) when is_binary(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:in_transaction(
      fun(_Db) ->
	      t_is_empty_devices_(AID) andalso
	      t_is_empty_specs_(AID) andalso
	      t_is_empty_users_(AID)
      end).

t_is_empty_devices_(AID) ->
    case exodm_db_device:list_next(AID, 1, <<"">>) of
	[] -> true;
	_ -> false
    end.

t_is_empty_specs_(AID) ->
    case exodm_db_yang:specs(AID) of
	done -> true;
	{[],F} ->  %% bug in kvdb:select??
	    case F() of
		done -> true;
		_ -> false
	    end;
	_ -> false
    end.

t_is_empty_users_(AID) ->
    case list_admins(AID, 2, <<"">>) of
	[Admin] ->
	    case list_users(AID, 2, <<"">>) of
		[Admin] -> true;
		_ -> false
	    end;
	_ -> false
    end.

%%--------------------------------------------------------------------
%% @doc
%% List N number of account starting after Prev.
%%
%% @end
%%--------------------------------------------------------------------
%% Hmm, don't know how to specify ??
%%-spec list_accounts(N::integer(), Prev::string() | binary()) ->
%%			   list(list({?ACC_DB_ID, Aid::binary()},
%%				     {?ACC_DB_NAME, Name::binary()})).

list_accounts(N, Prev) ->
    exodm_db:in_transaction(
      fun(_) ->
	      exodm_db:list_next(table(),
				 N, exodm_db:to_binary(Prev),
				 fun(Key) ->
					 lookup(Key)
				 end)
      end).


%%--------------------------------------------------------------------
%% @doc
%% List all account keys.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_account_keys() -> list(AID::binary()).
			       
list_account_keys() ->
    lists:reverse(fold_accounts(fun(AID, Acc) ->
					[AID|Acc]
				end, [])).

%%--------------------------------------------------------------------
%% @doc
%% List N number of account users starting after Prev
%%
%% @end
%%--------------------------------------------------------------------
-spec list_users(AID::binary(),
		 N::integer(),
		 Prev::binary()) ->
			list(UName::binary()) |
			  {error, Reason::term()}.

list_users(AID0, N, Prev) when is_binary(AID0) ->
    list_users_(exodm_db:account_id_key(AID0), N, Prev).

list_users_(AID,  N, Prev) ->
    FullPrev = kvdb_conf:join_key([AID, ?ACC_DB_USERS, exodm_db:to_binary(Prev)]),
    exodm_db:in_transaction(
      fun(_) ->
	      exodm_db:list_next(
		table(),
		N, FullPrev,
		fun(Key) ->
			lists:last(kvdb_conf:split_key(Key))
		end)
      end).

%%--------------------------------------------------------------------
%% @doc
%% List N number of account admins starting after Prev
%%
%% @end
%%--------------------------------------------------------------------
-spec list_admins(AName::binary(),
		  N::integer(),
		  Prev::binary()) ->
			 list(UName::binary()) |
				     {error, Reason::term()}.

list_admins(AID0, N, Prev) when is_binary(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    FullPrev = kvdb_conf:join_key([AID, ?ACC_DB_ADMINS, 
				   exodm_db:to_binary(Prev)]),
    exodm_db:in_transaction(
      fun(_) ->
	      exodm_db:list_next(table(),
				 N, FullPrev,
				 fun(Key) ->
					 lists:last(kvdb_conf:split_key(Key))
				 end)
      end).



%%--------------------------------------------------------------------
%% @doc
%% List all account roles
%%
%% @end
%%--------------------------------------------------------------------
-spec list_roles(AID::binary(),
		 N::integer(),
		 Prev::binary()) ->
			  ok |
			  {error, Reason::term()}.

list_roles(AID, N, Prev) when is_binary(AID) ->
    FullPrev = kvdb_conf:join_key([AID, ?ACC_DB_ROLES, 
				   exodm_db:to_binary(Prev)]),
    exodm_db:in_transaction(
      fun(_) ->
	      exodm_db:list_next(table(),
				 N, FullPrev,
				 fun(Key) ->
					 lists:last(kvdb_conf:split_key(Key))
				 end)
      end).


%%--------------------------------------------------------------------
%% @doc
%% List account roles for user
%%
%% @end
%%--------------------------------------------------------------------
-spec list_user_roles(AID::binary(), UName::binary()) ->
				list({AID::binary(), Role::binary()}).

list_user_roles(AID, UName) 
  when is_binary(AID), is_binary(UName) ->
    [R || {_A, R} <-
              lists:filter(fun({A, _R}) when A == AID -> true;
                              ({_OtherAID, _R}) -> false
                           end, exodm_db_user:list_roles(UName))].
    

%%--------------------------------------------------------------------
%% @doc
%% List account groups
%%
%% @end
%%--------------------------------------------------------------------
-spec list_groups(AID::binary()) ->
			 list().

list_groups(AID) when is_binary(AID) ->
    list_groups(AID, 30).

-spec list_groups(AID::binary(), Limit::integer()) ->
			 list().
list_groups(AID, Limit) when is_binary(AID) ->
    exodm_db_group:list_group_keys(AID, Limit).

%%--------------------------------------------------------------------
%% @doc
%% System specs are YANG specs exposing a custom ExoDM API for RPCs.
%% This could be used to enable premium services, or to allow custom
%% development for highly valued customers or beta customers.
%%
%% @end
%%--------------------------------------------------------------------
-spec system_specs(AID0::term()) ->
			  list().
%% Why not binary??
system_specs(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:in_transaction(
      fun(_) ->
	      Set = kvdb_conf:fold_children(
		      ?TAB,
		      fun(K, Acc) ->
			      [lists:last(exodm_db:split_key(K))|Acc]
		      end, [], exodm_db:join_key(AID, ?ACC_DB_SYSTEM_SPECS)),
	      lists:usort(Set ++ exodm_rpc:std_specs())
      end).

%%--------------------------------------------------------------------
%% Account user handling
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% Add account access to users
%%
%% @end
%%--------------------------------------------------------------------
-spec add_users(AName::binary(),
		Role::binary(),
		UNames::list(binary()), 
		IsRoot::boolean()) ->
		       ok |
		       {error, Reason::term()}.



add_users(AName, Role, UNames, IsRoot) 
  when is_binary(AName), is_binary(Role) ->
    lager:debug("aname ~p, role ~p, unames ~p", [AName, Role, UNames]),
    %% Check if account_exists
    case lookup_by_name(AName) of
	false -> error('object-not-found');
	AID -> add_users1(AID, Role, UNames, IsRoot)
    end.
		     
add_users1(AID, Role, UNames, true) ->
    %% Root - access is ok
    add_users2(AID, Role, UNames);
add_users1(AID, Role, UNames, false) ->
    case has_admin_access(AID) of
	true -> add_users2(AID, Role, UNames);
	false -> error('permission-denied')
    end.

add_users2(AID, Role, UNames) ->
    case role_exists(AID, Role) of
	true -> add_users3(AID, Role, UNames);
	false -> error('object-not-found')
    end.
    
add_users3(_AID, _Role, []) ->
    ok;
add_users3(_AID, ?ROOT, _UNames) ->
    error('permission-denied');
add_users3(_AID, ?INIT_ADMIN, _UNames) ->
    error('permission-denied');
add_users3(AID, Role, [UName | Rest]) ->
    case lists:any(fun({A, R}) when A == AID, R == Role -> true;
		      ({_A, _R}) -> false
		   end, exodm_db_user:list_roles(UName)) of
	true -> 
	    add_users3(AID, Role, Rest);
	false -> 
	    add_user(AID, UName, Role),
	    add_users3(AID, Role, Rest)
    end.

add_user(AID, UName, Role) 
  when Role == ?ADMIN;
       Role == ?INIT_ADMIN;
       Role == ?ROOT ->
    add_user1(AID, UName, Role),
    insert(exodm_db:join_key([AID, ?ACC_DB_ADMINS]), UName, <<>>);
add_user(AID, UName, Role) ->
    add_user1(AID, UName, Role).

add_user1(AID, UName, Role) ->
    lager:debug("aid ~p, uname ~p, role ~p", [AID, UName, Role]),
    exodm_db_user:add_role(AID, UName, Role),
    case user_exists(AID, UName) of
	true -> ok;
	false ->
	    kvdb_conf:write(table(), 
			    {exodm_db:join_key([AID,?ACC_DB_USERS,UName]), 
			     [], <<>>})
    end.

add_admin_user(AID, UName, true) ->
    add_user(AID, UName, ?ROOT);
add_admin_user(AID, UName, false) ->
    add_user(AID, UName, ?INIT_ADMIN).

%%--------------------------------------------------------------------
%% @doc
%% Remove account access from users
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_users(AName::binary(),
		   Role::binary(),
		   UNames::list(binary()), 
		   IsRoot::boolean()) ->
			  ok |
			  {error, Reason::term()}.



remove_users(AName, Role, UNames, IsRoot) 
  when is_binary(AName), is_binary(Role) ->
    lager:debug("aname ~p, role ~p, unames ~p", [AName, Role, UNames]),
    %% Check if account_exists
    case lookup_by_name(AName) of
	false -> error('object-not-found');
	AID -> remove_users1(AID, Role, UNames, IsRoot)
    end.
		     
remove_users1(AID, Role, UNames, true) ->
    %% Root - access is ok
    remove_users2(AID, Role, UNames);
remove_users1(AID, Role, UNames, false) ->
    case has_admin_access(AID) of
	true -> remove_users2(AID, Role, UNames);
	false -> error('permission-denied')
    end.


remove_users2(_AID, _Role, []) ->
    ok;
remove_users2(AID, Role, [UName | Rest]) ->
    lager:debug("aname ~p, role ~p, uname ~p", [AID, Role, UName]),
    case lists:any(fun({A, R}) when A == AID, R == Role -> true;
		      ({_A, _R}) -> false
		   end, exodm_db_user:list_roles(UName)) of
	true -> 
	    remove_user(AID, Role, UName),
	    remove_users2(AID, Role, Rest);
	false -> 
	    error('object-not-found')
    end.

remove_user(AID, ?ADMIN = Role, UName) ->
    kvdb_conf:delete(table(), kvdb_conf:join_key([AID, ?ACC_DB_ADMINS, UName])),
    remove_user1(AID, Role, UName);
remove_user(AID, Role, UName) ->
    remove_user1(AID, Role, UName).

remove_user1(AID, Role, UName) ->
    exodm_db_user:remove_role(AID, UName, Role),
    %% Was it the last role ??
    kvdb_conf:delete(table(), kvdb_conf:join_key([AID, ?ACC_DB_USERS, UName])).

%%--------------------------------------------------------------------
%% @doc
%% Check if user exsist
%%
%% @end
%%--------------------------------------------------------------------
-spec user_exists(AID::binary(), UName::binary()) ->
			 Exists::boolean().

user_exists(AID, UName) when is_binary(AID), is_binary(UName) ->
    case kvdb_conf:read(?TAB, exodm_db:join_key([AID, ?ACC_DB_USERS, UName])) of
	{ok, _User} -> true;
	{error, not_found} -> false
    end.
	    
%%--------------------------------------------------------------------
%% Account role handling
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% Create an account role
%%
%% <AID>/roles/<Role>/name
%%                   /descr
%%                   /groups/<gid> = role()
%% Can throw errors: no_such_group
%% @end
%%--------------------------------------------------------------------
-spec create_role(AID::binary(), RName::binary(), Opts::list()) ->
			 ok.

create_role(AID, RName, Opts) 
  when is_binary(AID), is_binary(RName), is_list(Opts) ->
    exodm_db:in_transaction(
      fun(_) ->
	      t_create_role(AID, RName, Opts)
      end).

t_create_role(AID0, RName, Opts) ->
    AID = exodm_db:account_id_key(AID0),
    check_access(AID),
    t_create_role_(AID, RName, Opts).

t_create_role_(AID, RName, Opts) ->
    Key = role_key(AID, RName),
    insert(Key, ?ROLE_DB_DESCR, binary_opt(?ROLE_OPT_DESCR, Opts)),
    %% Add an exist field for later existence checks.
    insert(Key, ?ROLE_DB_EXISTS, <<"true">>),
    AccessKey = exodm_db:join_key(Key, ?ROLE_DB_ACCESS),
    lists:foreach(
      fun(<<"predefined">>) ->
              insert(AccessKey, <<"predefined">>, <<>>);
         ({<<"all">>, Access}) ->
	      insert(AccessKey, <<"all">>, Access);
	 ({G, Access}) ->
	      case group_exists(AID, G) of
		  true ->
		      insert(AccessKey, G, Access);
		  false ->
		      error({no_such_group, [AID, G]})
	      end
      end, proplists:get_all_values(?ROLE_OPT_ACCESS, Opts)),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Get role data from db
%%
%% @end
%%--------------------------------------------------------------------
-spec role_def(AID::binary(), RName::binary()) ->
		      list() |
		      {error, not_found}.

role_def(AID, RName) ->
    exodm_db:in_transaction(
      fun(_) ->
	      case kvdb_conf:read_tree(?TAB, role_key(AID, RName)) of
		  {conf_tree, _Key, Def} -> Def;
		  _ -> {error, not_found}
	      end
      end).

%%--------------------------------------------------------------------
%% @doc
%% Check if role exists
%%
%% @end
%%--------------------------------------------------------------------
-spec role_exists(AID::binary(), RName::binary()) ->
		      Exists::boolean().

role_exists(AID, RName) when is_binary(AID), is_binary(RName) ->
    %% Role has a field exists
    case read(role_key(AID, RName), ?ROLE_DB_EXISTS) of
	[] -> false;
	ValueList when is_list(ValueList) -> true
    end.

%%--------------------------------------------------------------------
%% @doc
%% Get roles with permission to execute rpc.
%% Only implemented for exodm yang module.
%%
%% @end
%%--------------------------------------------------------------------
-spec rpc_roles(AID::binary(), Rpc::binary()) ->
                       Permission::boolean().

rpc_roles(AID, Rpc) when is_binary(AID), is_binary(Rpc) ->
    %% Only implemented for exodm yang module. !!!!
    case lists:keyfind(Rpc, 1, ?RPC_ROLE_LIST) of
        {Rpc, RoleList} -> RoleList;
        false ->
            lager:debug("Warning, unknown rpc ~p", Rpc),
            error('object-not-found')
    end.
                
%%--------------------------------------------------------------------
%% @doc
%% Check if role has permission to execute rpc.
%%
%% @end
%%--------------------------------------------------------------------
-spec rpc_permission(Rpc::binary(), Roles::binary() | list(binary())) ->
                            Permission::boolean().

rpc_permission(Rpc, Roles) when is_binary(Rpc), is_list(Roles) ->
    case lists:keyfind(Rpc, 1, ?RPC_ROLE_LIST) of
        {Rpc, RoleList} ->
            case Roles -- (Roles -- RoleList) of
                [] -> false;
                _RList -> true
            end;
        false ->
            lager:debug("Warning, unknown rpc ~p", binary_to_atom(Rpc,latin1)),
            false
    end;
rpc_permission(Rpc, Role) when is_binary(Rpc) ->
    case lists:keyfind(Rpc, 1, ?RPC_ROLE_LIST) of
        {Rpc, RoleList} ->
            lists:member(Role, RoleList);
        false ->
            lager:debug("Warning, unknown rpc ~p", binary_to_atom(Rpc,latin1)),
            false
    end.
                
     
%%--------------------------------------------------------------------
%% @doc
%% Returns the list of predefined role rpc permissions..
%% Rework when better storage of permissions.
%% How to handle customer rpc:s ??
%%
%% @end
%%--------------------------------------------------------------------
-spec rpc_role_list() -> list({Rpc::binary(), list(Role::binary())}).

rpc_role_list() ->   
    ?RPC_ROLE_LIST.

%%--------------------------------------------------------------------
create_roles(AID, true) ->
    create_admin_role(AID, ?ROOT);
create_roles(AID,false) ->
    create_admin_role(AID, ?INIT_ADMIN),
    DefaultRoles = configurable_roles(),
    lists:foldl(fun(Role, _Acc) ->
			t_create_role_(AID, Role,initial_role_opts(Role))
		end, [], DefaultRoles).

create_admin_role(AID, RName) ->
    t_create_role_(AID, RName, initial_role_opts(RName)).
 
configurable_roles() ->
    %% Later add user defined roles??
    [?ADMIN, ?EXEC, ?CONFIG, ?VIEW].


%% Future access format is {<<"all">> | GroupName, [list(Rpc)]}
initial_role_opts(?ROOT) ->
    [{?ROLE_OPT_DESCR, "initial root user"},
     {?ROLE_OPT_ACCESS, <<"predefined">>}];
initial_role_opts(?INIT_ADMIN) ->
    [{?ROLE_OPT_DESCR, "initial administrator - can not be removed"},
     {?ROLE_OPT_ACCESS, <<"predefined">>}];
initial_role_opts(?ADMIN) ->
    [{?ROLE_OPT_DESCR, "administrator of account"},
     {?ROLE_OPT_ACCESS, <<"predefined">>}];
initial_role_opts(?EXEC) ->
    [{?ROLE_OPT_DESCR, "user of account, can call exec rpc:s"},
     {?ROLE_OPT_ACCESS, <<"predefined">>}];
initial_role_opts(?CONFIG) ->
    [{?ROLE_OPT_DESCR, "user of account, can call config rpc:s"},
     {?ROLE_OPT_ACCESS, <<"predefined">>}];
initial_role_opts(?VIEW) ->
    [{?ROLE_OPT_DESCR, "user of account, can call list rpc:s"},
     {?ROLE_OPT_ACCESS, <<"predefined">>}].


has_admin_access(AID) ->
    %% Check if current user has admin rights for account
    Client = to_binary(exodm_db_session:get_user()),
    lists:any(fun({A, ?ADMIN}) when A == AID -> true;
		 ({A, ?INIT_ADMIN}) when A == AID -> true;
		 ({A, ?ROOT}) when A == AID -> true;
		 ({_A, _R}) -> false
	      end, exodm_db_user:list_roles(Client)).



%%--------------------------------------------------------------------
%%% Account group handling
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% Check if role exists
%%
%% @end
%%--------------------------------------------------------------------
-spec group_exists(AID::binary(), GID::binary()) ->
		      Exists::boolean().

group_exists(AID, GID) when is_binary(AID), is_binary(GID) ->
    case exodm_db:read(?TAB, exodm_db:join_key([AID, ?ACC_DB_GROUPS,
						GID, ?ACC_DB_GROUP_NAME])) of
	[] ->
	    false;
	_ ->
	    true
    end.

%%--------------------------------------------------------------------
%%% Account protocol handling
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% Add protocol to account
%%
%% Can throw errors: unknown_protocol
%% @end
%%--------------------------------------------------------------------
-spec register_protocol(AID::binary(), Protocol::binary()) ->
			       ok.

register_protocol(AID, Protocol) 
  when is_binary(AID), is_binary(Protocol) ->
    case exodm_rpc_protocol:module(Protocol) of
	undefined ->
	    erlang:error({unknown_protocol, Protocol});
	_ ->
	    exodm_db:in_transaction(
	      fun(_) ->
		      Key = exodm_db:join_key(exodm_db:account_id_key(AID),
					      ?ACC_DB_PROTS),
		      insert(Key, Protocol, <<>>)
	      end)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Check if protocol is added to account
%%
%% @end
%%--------------------------------------------------------------------
-spec is_protocol_registered(AID::binary(), Protocol::binary()) ->
			       Registered::boolean().

is_protocol_registered(AID, Protocol) 
  when is_binary(AID), is_binary(Protocol) ->
    exodm_db:in_transaction(
      fun(_) ->
	      case kvdb_conf:read(?TAB, exodm_db:join_key(
					  [exodm_db:account_id_key(AID),
					  ?ACC_DB_PROTS, Protocol])) of
		  {ok, _} ->
		      true;
		  {error, _} ->
		      false
	      end
      end).


%%--------------------------------------------------------------------
%%% Account counters handling
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% Increment request id counter
%%
%% @end
%%--------------------------------------------------------------------
-spec incr_request_id(AID0::term()) ->
			     ok.

incr_request_id(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    kvdb_conf:update_counter(?TAB, 
			     exodm_db:join_key(AID, ?ACC_DB_LAST_REQ), 1).

%%--------------------------------------------------------------------
%% @doc
%% Increment transaction id counter
%%
%% @end
%%--------------------------------------------------------------------
-spec incr_transaction_id(AID0::term()) ->
			     ok.

incr_transaction_id(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    kvdb_conf:update_counter(?TAB, exodm_db:join_key(AID, ?ACC_DB_LAST_TID), 1).


%%--------------------------------------------------------------------
%% Utils
%%--------------------------------------------------------------------

role_key(AID, RID) ->
    exodm_db:join_key([exodm_db:account_id_key(AID),
		       ?ACC_DB_ROLES,
		       exodm_db:encode_id(RID)]).

insert(Key, Item, Value) ->
    Key1 = exodm_db:join_key([Key, Item]),
    exodm_db:write(?TAB, Key1, Value).

read(Key,Item) ->
    Key1 = exodm_db:join_key([Key, Item]),
    case exodm_db:read(?TAB, Key1) of
	{ok,{_,_,Value}} -> [{Item,Value}];
	{error,not_found} -> []
    end.

check_access(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    UName = exodm_db_session:get_user(),
    lager:debug("aid ~p, uname ~p", [AID, UName]),
    if UName ==	root -> AID;
       true ->
	    case exodm_db:read(
		   ?TAB,exodm_db:join_key([AID, ?ACC_DB_ADMINS,
					   exodm_db:encode_id(UName)])) of
		{ok, _}   -> AID;
		{error,_Error} -> 
		    lager:debug("error ~p", [_Error]),
		    error(not_authorized)
	    end
    end.

%%--------------------------------------------------------------------
%% NOT USED ??
%%--------------------------------------------------------------------
fold_accounts(F, Acc) ->
    kvdb_conf:fold_children( ?TAB, F, Acc, <<>>).

%%--------------------------------------------------------------------
%%% EUnit
%%--------------------------------------------------------------------
-ifdef(TEST).

acct_test_() ->
    {setup,
     fun() ->
	     ?debugVal(application:start(gproc)),
	     ?debugVal(application:start(crypto)),
	     ?debugVal(application:start(bcrypt)),
	     ?debugVal(application:start(kvdb)),
	     ?debugVal(kvdb_conf:open(undefined, [{backend, ets}])),
	     ?debugVal(exodm_db:init())
     end,
     fun(_) ->
	     application:stop(kvdb),   application:unload(kvdb),
	     application:stop(gproc),  application:unload(gproc),
	     application:stop(bcrypt), application:unload(bcrypt),
	     application:stop(crypto), application:unload(crypto)

     end,
     [
      {timeout, 60000, [?_test(?dbg(test_new()))]}
     ]}.

test_new() ->
    ok = new([{name, <<"test1">>},
		     {admin, [{uname, <<"u1">>},
			      {alias, <<"u">>},
			      {fullname, <<"Mr U">>},
			      {password, <<"pwd">>}]}]),
    AllTabs = kvdb:list_tables(kvdb_conf),
    AID = lookup_by_name(<<"test1">>),
    lists:all(fun(T) ->
		      lists:member(T, AllTabs)
	      end, [exodm_db_device:table(AID),
		    exodm_db_group:table(AID),
		    exodm_db_config:table(AID),
		    exodm_db_device_type:table(AID)]),
    ok.

-endif.
