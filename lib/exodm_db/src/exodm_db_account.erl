%%% @author Ulf Wiger <ulf@feuerlabs.com>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense device manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_account).

-include_lib("lager/include/log.hrl").

-export([new/1, 
	 update/2, 
	 lookup/1, 
	 lookup_by_name/1, 
	 exist/1,
	 is_empty/1,
	 delete/1]).
-export([list_accounts/2,
	 list_account_keys/0,
	 list_roles/3,
	 list_users/3,
	 list_users/4,
	 list_admins/3]).
-export([add_users/4,
	 remove_users/3]).
-export([create_role/3,
	 role_def/2,
	 role_exists/2]).
-export([register_protocol/2, 
	 is_protocol_registered/2]).
-export([create_exodm_account/0]).
-export([fold_accounts/2]).
%%-export([list_users/1, list_users/2]).
-export([list_groups/1, list_groups/2]).
-export([system_specs/1]).
%%-export([list_admins/1]).
-export([incr_request_id/1,
	 incr_transaction_id/1]).
-export([subscribe/1,
	 unsubscribe/1]).
-export([key/1,
	 table/0]).
-export([init/0]).

-import(exodm_db, [binary_opt/2, to_binary/1]).

-define(TAB, <<"acct">>).

%% Predefined roles
-define(ROOT, <<"root">>).
-define(INIT_ADMIN, <<"initial_admin">>).
-define(ADMIN, <<"admin">>).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("../test/feuerlabs_eunit.hrl").
-endif.

init() ->
    exodm_db:in_transaction(
      fun(_) ->
	      exodm_db:add_table(?TAB, [name])
      end).

table() ->
    ?TAB.


%% Gproc pub/sub ----------------------
subscribe(Event) when Event==add; Event==delete ->
    gproc:reg({p, l, {?MODULE, Event}}).

unsubscribe(Event) when Event==add; Event==delete ->
    catch gproc:unreg({p, l, {?MODULE, Event}}),
    ok.

publish(Event, Data) when Event==add; Event==delete ->
    gproc:send({p, l, {?MODULE, Event}}, {?MODULE, Event, Data}).
%% END Gproc pub/sub ------------------


%%
%% Setup exodm account if needed
%% called from exodm_db:init - after other init!
%%
create_exodm_account() ->
    exodm_db:in_transaction(
      fun(Db) ->
	      case lookup_by_name(<<"exodm">>) of
		  [] ->
		      create_exodm_account_(Db);
		  [_] ->
		      ok
	      end
      end).

create_exodm_account_(_Db) ->
    exodm_db_session:set_trusted_proc(),
    R=
	new(
	  [
	   {name, <<"exodm">>},
	   {root, true},
	   {admin, [
		   {uname, <<"exodm-admin">>},
		   {fullname, <<"Administrator">>},
		   {password, atom_to_binary(erlang:get_cookie(),latin1)}
		   ]}]),
    exodm_db_session:unset_trusted_proc(),
    R.
    
%%
%% /<aid>/name  = <AccountName>
%% /<aid>/admin = <UID>
%%

%% FIXME option validation
new(Options) ->
    exodm_db:in_transaction(
      fun(_) ->
	      AIDVal = new_(Options),
	      publish(add, AIDVal),
	      AIDVal
      end).

new_(Options) ->
    %% initial check: there must be an admin
    UserOpts = case lists:keyfind(admin, 1, Options) of
		   false ->
		       error(no_admin_user);
		   {_, Os} when is_list(Os) ->
		       Os
	       end,
    Root = 
	case exodm_db_session:is_trusted_proc() of
	    true -> proplists:get_bool(root, Options);
	    false -> false
	end,
    AID = exodm_db_system:new_aid(),
    Key = exodm_db:escape_key(AID),
    exodm_db_group:init(AID),
    exodm_db_device_type:init(AID),
    %%insert(Key, '__last_rid', <<0:32>>),
    insert(Key, '__last_req_id', <<0:32>>),
    insert(Key, '__last_tid', <<0:32>>),
    AdminUName = binary_opt(uname, UserOpts),
    AcctName = case binary_opt(name, Options) of
		   <<>> -> AdminUName;
		   Other -> Other
	       end,
    insert(Key, name, AcctName),
    {ok, AdminUName} = exodm_db_user:new(AdminUName, UserOpts),
    create_roles(AID, AdminUName, Root),
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

%% delete the account, caller must make sure that data
%% access via this account is first deleted.
delete(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:in_transaction(
      fun(_Db) ->
	      [Admin] = list_admins(AID, 2, <<"">>),
	      exodm_db_user:delete(Admin),
	      kvdb_conf:delete_tree(table(), AID),
	      publish(delete, exodm_db:account_id_value(AID)),
	      ok
      end).

%% list N number of account starting after Prev
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



add_users(AName, Role, UNames, IsRoot) ->
    lager:debug("aname ~p, role ~p, unames ~p", [AName, Role, UNames]),
    %% Check if account_exists
    case exodm_db_account:lookup_by_name(AName) of
	[] -> error('object-not-found');
	[AID] -> add_users1(AID, Role, UNames, IsRoot)
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
    %% Check if role exists
    Roles = list_roles(AID, 100, ""), %% All ??
    case lists:member(Role, Roles) of
	true -> add_users3(AID, Role, UNames);
	false -> error('object-not-found')
    end.
    
add_users3(_AID, _Role, []) ->
    ok;
add_users3(_AID, ?ROOT, _UNames) ->
    error('permission-denied');
add_users3(_AID, ?INIT_ADMIN, _UNames) ->
    error('permission-denied');
add_users3(AID, ?ADMIN = Role, [UName | Rest]) ->
    add_user(AID, UName, Role),
    insert(exodm_db:join_key(exodm_db:escape_key(AID), <<"admins">>), 
	   UName, UName),
    add_users3(AID, Role, Rest);
add_users3(AID, Role, [UName | Rest]) ->
    add_user(AID, UName, Role),
    add_users3(AID, Role, Rest).

add_user(AID, UName, Role) ->
    exodm_db_user:add_access(AID, UName, Role),
    kvdb_conf:write(table(), 
		    {kvdb_conf:join_key([AID,<<"users">>,UName]), [], <<>>}).

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
		   UNames::list(binary()), 
		   IsRoot::boolean()) ->
			  ok |
			  {error, Reason::term()}.



remove_users(AName, UNames, IsRoot) ->
    lager:debug("aname ~p, unames ~p", [AName, UNames]),
    %% Check if account_exists
    case exodm_db_account:lookup_by_name(AName) of
	[] -> error('object-not-found');
	[AID] -> remove_users1(AID, UNames, IsRoot)
    end.
		     
remove_users1(AID, UNames, true) ->
    %% Root - access is ok
    remove_users2(AID, UNames);
remove_users1(AID, UNames, false) ->
    case has_admin_access(AID) of
	true -> remove_users2(AID, UNames);
	false -> error('permission-denied')
    end.


remove_users2(_AID, []) ->
    ok;
remove_users2(AID, [UName | Rest]) ->
    lager:debug("aname ~p, uname ~p", [AID, UName]),
    case lists:keyfind(AID, 1, exodm_db_user:list_access(UName)) of
	false -> error('object-not-found');
	{AID, ?INIT_ADMIN} -> error('permission-denied');
	{AID, ?ROOT} -> error('permission-denied');
	{AID, Role} -> 
	    remove_user(AID, UName, Role),
	    remove_users2(AID, Rest)
    end.

remove_user(AID, UName, ?ADMIN = Role) ->
    kvdb_conf:delete(table(), kvdb_conf:join_key([AID, <<"admins">>, UName])),
    remove_user1(AID, UName, Role);
remove_user(AID, UName, Role) ->
    remove_user1(AID, UName, Role).

remove_user1(AID, UName, Role) ->
    exodm_db_user:remove_access(AID, UName),
    kvdb_conf:delete(table(), kvdb_conf:join_key([AID, <<"users">>, UName])).

%%--------------------------------------------------------------------
%% @doc
%% List account users
%%
%% @end
%%--------------------------------------------------------------------
-spec list_users(AName::binary(),
		 N::integer(),
		 Prev::binary(),
		 IsRoot::boolean()) ->
			  ok |
			  {error, Reason::term()}.



list_users(AName, N, Prev, IsRoot) ->
    lager:debug("aname ~p, n ~p, prev ~p", [AName, N, Prev]),
    %% Check if account_exists
    case exodm_db_account:lookup_by_name(AName) of
	[] -> error('object-not-found');
	[AID] -> list_users1(AID, N, Prev, IsRoot)
    end.

list_users1(AID, N, Prev, true) ->
    %% Root - access is ok
    list_users_(AID, N, Prev);
list_users1(AID, N, Prev, false) ->
    case has_admin_access(AID) of
	true -> list_users_(AID, N, Prev);
	false -> error('permission-denied')
    end.

has_admin_access(AID) ->
    %% Check if current user has admin rights for account
    Client = to_binary(exodm_db_session:get_user()),
    AccountAdmins = list_admins(AID, 100, ""), %% All?
    lists:member(Client, AccountAdmins).

create_roles(AID, AdminUName, true) ->
    create_admin_role(AID, AdminUName, ?ROOT);
create_roles(AID, AdminUName, false) ->
    create_admin_role(AID, AdminUName, ?INIT_ADMIN),
    DefaultRoles = configurable_roles(),
    lists:foldl(fun(Role, _Acc) ->
			t_create_role_(AID, Role,initial_role_opts(Role))
		end, [], DefaultRoles).

create_admin_role(AID, AdminUName, RName) ->
    t_create_role_(AID, RName, initial_role_opts(RName)),
    exodm_db_user:add_access(AID, AdminUName, RName),
    UNameKey = to_binary(AdminUName),
    insert(exodm_db:join_key(exodm_db:escape_key(AID), <<"admins">>), 
	   UNameKey, AdminUName).

%% Note!!! These must correspond to roles defined in exodm_type.yang !!!
configurable_roles() ->
    %% Later add user defined roles??
    [<<"admin">>,<<"executer">>,<<"configurer">>,<<"viewer">>].


%% Future access format is {<<"all">> | GroupName, [list(Rpc)]}
initial_role_opts(?ROOT) ->
    [{descr, "initial root user"},
     {access, {<<"all">>,<<"root">>}}];
initial_role_opts(?INIT_ADMIN) ->
    [{descr, "initial administrator - can not be removed"},
     {access, {<<"all">>, <<"admin">>}}];
initial_role_opts(?ADMIN) ->
    [{descr, "administrator of account"},
     {access, {<<"all">>, <<"admin">>}}];
initial_role_opts(<<"executer">>) ->
    [{descr, "user of account, can call exec rpc:s"},
     {access, {<<"all">>, <<"exec">>}}];
initial_role_opts(<<"configurer">>) ->
    [{descr, "user of account, can call config rpc:s"},
     {access, {<<"all">>, <<"config">>}}];
initial_role_opts(<<"viewer">>) ->
    [{descr, "user of account, can call list rpc:s"},
     {access, {<<"all">>, <<"view">>}}].




%% <AID>/roles/<Role>/name
%%                   /descr
%%                   /groups/<gid> = access()
create_role(AID, RName, Opts) ->
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
    insert(Key, descr, binary_opt(descr, Opts)),
    AccessKey = exodm_db:join_key(Key, <<"access">>),
    lists:foreach(
      fun({<<"all">>, Access}) ->
	      valid_access(Access),
	      insert(AccessKey, <<"all">>, Access);
	 ({G, Access}) ->
	      valid_access(Access),
	      case group_exists(AID, G) of
		  true ->
		      insert(AccessKey, G, Access);
		  false ->
		      error({no_such_group, [AID, G]})
	      end
      end, proplists:get_all_values(access, Opts)),
    ok.

list_roles(AID0, N, Prev) ->
    AID = exodm_db:account_id_key(AID0),
    FullPrev = kvdb_conf:join_key([AID, <<"roles">>, 
				   exodm_db:to_binary(Prev)]),
    exodm_db:in_transaction(
      fun(_) ->
	      exodm_db:list_next(table(),
				 N, FullPrev,
				 fun(Key) ->
					 lists:last(kvdb_conf:split_key(Key))
				 end)
      end).


role_def(AID, RName) ->
    exodm_db:in_transaction(
      fun(_) ->
	      kvdb_conf:read_tree(?TAB, exodm_db:join_key(
					  [exodm_db:account_id_key(AID),
					   <<"roles">>, 
					   exodm_db:encode_id(RName)]))
      end).

role_exists(AID, Role0) ->
    Role = exodm_db:encode_id(to_binary(Role0)),
    exist(exodm_db:join_key([exodm_db:account_id_key(AID),
			     <<"roles">>, Role])).

valid_access(<<"view">>) -> true;
valid_access(<<"config">>) -> true;
valid_access(<<"exec">>) -> true;
valid_access(<<"admin">>) -> true;
valid_access(<<"root">>) ->
    case exodm_db_session:is_trusted_proc() of
	true -> true;
	false -> error({invalid_access, <<"root">>})
    end;
valid_access(A) ->
    error({invalid_access, A}).

%% add_user(AID0, UName, RID) ->
%%     AID = check_access(AID0),
%%     add_user_(AID, UName, RID).

%% add_user_(AID0, UName0, RID) ->
%%     UName = exodm_db:encode_id(UName0),
%%     case exodm_db_user:exist(UName) of
%% 	true ->
%% 	    AID = exodm_db:account_id_key(AID0),
%% 	    Key = exodm_db:join_key(
%% 		    [AID, <<"users">>, UName]),
%% 	    case read(Key, '__uname') of
%% 		[] ->
%% 		    insert(Key, '__uname', UName0),
%% 		    insert(Key, '__rid', rid_value(RID)),
%% 		    ok;
%% 		{ok, _} ->
%% 		    {error, exists}
%% 	    end;
%% 	false ->
%% 	    error(unknown_user, [AID0, UName0])
%%     end.

%% FIXME validate every item BEFORE insert!
update(AID, Options) ->
    exodm_db:in_transaction(
      fun(_) ->
	      t_update(AID, Options)
      end).

t_update(AID0, Options) ->
    AID = check_access(AID0),
    update_(AID, Options).

update_(Key = AID, Options) ->
    F =	fun({name,Value}) ->
		NewName = to_binary(Value),
		insert(Key, name, NewName);
	   ({add_admin,UName}) ->
		update_add_admin_(AID, UName)
	end,
    lists:foreach(F, Options).

update_add_admin_(AID, UName) ->
    case exodm_db_user:exist(UName) of
	false ->
	    error({no_such_user, UName});
	true ->
	    Last = exodm_db:fold_list(
		     ?TAB,
		     fun(I, LKey, _Acc) ->
			     case exist(exodm_db:join_key(
					  LKey, <<"__uname">>)) of
				 true ->
				     error({admin_exists, UName});
				 false ->
				     I
			     end
		     end, 0,
		     exodm_db:join_key(AID, <<"admin">>)),
	    insert(
	      exodm_db:join_key(
		AID, exodm_db:list_key(admin, Last+1)), '__uname', UName)
    end.


lookup(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    lookup_(key(AID)).

lookup_(Key) ->
    <<_,ID/binary>> = lists:last(exodm_db:split_key(Key)),
    [{id,ID}] ++
	read(Key,name) ++
	read(Key, admin).

lookup_by_name(Name) ->
    Matches = kvdb:index_get(kvdb_conf, ?TAB, name, Name),
    %% e.g. [{<<"a00000001*name">>,[],<<"getaround">>}]
    lists:foldr(
      fun({Key, _, _}, Acc) ->
	      case exodm_db:split_key(Key) of
		  [ID, _] -> [ID|Acc];
		  _       -> Acc
	      end
      end, [], Matches).

%% get_admin_uid(Admin) ->
%%     case exodm_db_system:get_uid_user(Admin) of
%% 	{error, not_found} ->
%% 	    case exodm_db_user:lookup_attr(Admin, <<"__uid">>) of
%% 		{error, not_found} ->
%% 		    {error, not_found};
%% 		[UID] ->
%% 		    UID
%% 	    end;
%% 	_ ->
%% 	    Admin
%%     end.

exist(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    exist_(key(AID)).

exist_(Key) ->
    case read(Key,name) of
	[] -> false;
	[_] -> true
    end.

list_users(AID0, N, Prev) ->
    list_users_(exodm_db:account_id_key(AID0), N, Prev).

list_users_(AID,  N, Prev) ->
    FullPrev = kvdb_conf:join_key([AID, <<"users">>, exodm_db:to_binary(Prev)]),
    exodm_db:in_transaction(
      fun(_) ->
	      exodm_db:list_next(
		table(),
		N, FullPrev,
		fun(Key) ->
			lists:last(kvdb_conf:split_key(Key))
		end)
      end).

list_admins(AID0, N, Prev) ->
    AID = exodm_db:account_id_key(AID0),
    FullPrev = kvdb_conf:join_key([AID, <<"admins">>, 
				   exodm_db:to_binary(Prev)]),
    exodm_db:in_transaction(
      fun(_) ->
	      exodm_db:list_next(table(),
				 N, FullPrev,
				 fun(Key) ->
					 lists:last(kvdb_conf:split_key(Key))
				 end)
      end).



list_account_keys() ->
    lists:reverse(fold_accounts(fun(AID, Acc) ->
					[AID|Acc]
				end, [])).

%%% Group handling
group_exists(AID, GID) ->
    case exodm_db:read(?TAB, exodm_db:join_key([AID, <<"groups">>,
						GID, <<"name">>])) of
	[] ->
	    false;
	_ ->
	    true
    end.

%%% Protocol handling
register_protocol(AID, Protocol) when is_binary(Protocol) ->
    case exodm_rpc_protocol:module(Protocol) of
	undefined ->
	    erlang:error({unknown_protocol, Protocol});
	_ ->
	    exodm_db:in_transaction(
	      fun(_) ->
		      Key = exodm_db:join_key(exodm_db:account_id_key(AID),
					      <<"protocols">>),
		      insert(Key, Protocol, <<>>)
	      end)
    end.

is_protocol_registered(AID, Protocol) ->
    exodm_db:in_transaction(
      fun(_) ->
	      case kvdb_conf:read(?TAB, exodm_db:join_key(
					  [exodm_db:account_id_key(AID),
					   <<"protocols">>, Protocol])) of
		  {ok, _} ->
		      true;
		  {error, _} ->
		      false
	      end
      end).

%% Check if account is empty
%% no device must exist
%% no yang specs must exit
%%
is_empty(AID0) ->
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
	{[],F} ->  %% bug in kvdb:select?
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

%% FIXME!!!! remove? exodm_db:fold_keys does not exist.
list_users(AID) ->
    list_users(AID, 30).

list_users(AID0, Limit) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:fold_keys(
      ?TAB,
      exodm_db:join_key(AID, <<"users">>),
      fun([A, <<"users">>, UID|_], Acc) ->
	      {next, exodm_db:join_key([A,<<"users">>, UID]), [UID|Acc]};
	 (_, Acc) ->
	      {done, Acc}
      end, [], Limit).

list_groups(AID) ->
    list_groups(AID, 30).

list_groups(AID, Limit) ->
    exodm_db_group:list_group_keys(AID, Limit).

%% System specs are YANG specs exposing a custom ExoDM API for RPCs.
%% This could be used to enable premium services, or to allow custom
%% development for highly valued customers or beta customers.
%%
system_specs(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:in_transaction(
      fun(_) ->
	      Set = kvdb_conf:fold_children(
		      ?TAB,
		      fun(K, Acc) ->
			      [lists:last(exodm_db:split_key(K))|Acc]
		      end, [], exodm_db:join_key(AID, <<"system_specs">>)),
	      lists:usort(Set ++ exodm_rpc:std_specs())
      end).

%% FIXME!!!! remove? exodm_db:fold_keys does not exist.
list_admins(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:fold_keys(
      ?TAB,
      exodm_db:join_key(AID, <<"admins">>),
      fun([UName|_], Acc) ->
	      [UName|Acc]
      end, [], 100).

fold_accounts(F, Acc) ->
    kvdb_conf:fold_children( ?TAB, F, Acc, <<>>).

%% utils

key(AName) ->
    to_binary(AName).
%% exodm_db:join_key(<<"account">>, AName).

role_key(AID, RID) ->
    exodm_db:join_key([exodm_db:account_id_key(AID),
		       <<"roles">>,
		       exodm_db:encode_id(RID)]).

new_role_id(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:update_counter( ?TAB,
			     exodm_db:join_key(AID, <<"__last_rid">>), 1).

insert(Key, Item, Value) ->
    Key1 = exodm_db:join_key([Key, to_binary(Item)]),
    exodm_db:write(?TAB, Key1, Value).

read(Key,Item) ->
    Key1 = exodm_db:join_key([Key, to_binary(Item)]),
    case exodm_db:read(?TAB, Key1) of
	{ok,{_,_,Value}} -> [{Item,Value}];
	{error,not_found} -> []
    end.

check_access(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    UName = exodm_db_session:get_user(),
    lager:debug("aid ~p, uname ~p", [AID, UName]),
    case UName of
	root -> AID;
	_ ->
	    case exodm_db:read(
		   ?TAB,exodm_db:join_key([AID, <<"admins">>,
					   exodm_db:encode_id(UName)])) of
		{ok, _}   -> AID;
		{error,_Error} -> 
		    lager:debug("error ~p", [_Error]),
		    error(not_authorized)
	    end
    end.

only_child(Tab, Parent, UID) ->
    case kvdb_conf:first_child(Tab, Parent) of
	{ok, Key} ->
	    case lists:reverse(kvdb_conf:split_key(Key)) of
		[UID|_] ->
		    case kvdb_conf:next_child(Tab, Key) of
			done ->
			    true;
			{ok, _} ->
			    false
		    end;
		_ ->
		    false
	    end;
	done ->
	    %% Shouldn't happen, but strictly speaking, UID is not the only
	    %% child.
	    false
    end.


incr_request_id(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    kvdb_conf:update_counter(?TAB, 
			     exodm_db:join_key(AID, <<"__last_req_id">>), 1).

incr_transaction_id(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    kvdb_conf:update_counter(?TAB, exodm_db:join_key(AID, <<"__last_tid">>), 1).


%%% EUnit
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
    {ok, AID} = new([{name, <<"test1">>},
		     {admin, [{uname, <<"u1">>},
			      {alias, <<"u">>},
			      {fullname, <<"Mr U">>},
			      {password, <<"pwd">>}]}]),
    AllTabs = kvdb:list_tables(kvdb_conf),
    lists:all(fun(T) ->
		      lists:member(T, AllTabs)
	      end, [exodm_db_device:table(AID),
		    exodm_db_group:table(AID),
		    exodm_db_config:table(AID),
		    exodm_db_device_type:table(AID)]),
    ok.

-endif.
