%%% @author Ulf Wiger <ulf@feuerlabs.com>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense device manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_account).

-export([new/1, update/2, lookup/1, lookup_by_name/1, exist/1]).
-export([is_empty/1, delete/1]).
-export([create_role/3]).
-export([create_exodm_account/0]).
-export([register_protocol/2, is_protocol_registered/2]).
-export([list_accounts/2,
	 list_users/3,
	 list_admins/3]).
-export([list_account_keys/0]).
-export([fold_accounts/2]).
-export([list_users/1, list_users/2]).
-export([list_groups/1, list_groups/2]).
-export([system_specs/1]).
-export([list_admins/1]).
-export([incr_request_id/1,
	 incr_transaction_id/1]).
-export([key/1,
	 table/0]).
-export([init/0]).

%% callbacks
-export([add_user/2,
	 remove_user/2]).

-import(exodm_db, [binary_opt/2, to_binary/1]).

-define(TAB, <<"acct">>).

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
	      new_(Options)
      end).

new_(Options) ->
    %% initial check: there must be an admin
    UserOpts = case lists:keyfind(admin, 1, Options) of
		   false ->
		       error(no_admin_user);
		   {_, Os} when is_list(Os) ->
		       Os
	       end,
    Root = case exodm_db_session:is_trusted_proc() of
	       true -> proplists:get_bool(root, Options);
	       false -> false
	   end,
    AID = exodm_db_system:new_aid(),
    Key = exodm_db:escape_key(AID),
    exodm_db_group:init(AID),
    exodm_db_device_type:init(AID),
    insert(Key, '__last_rid', <<0:32>>),
    insert(Key, '__last_req_id', <<0:32>>),
    insert(Key, '__last_tid', <<0:32>>),
    AdminUName = binary_opt(uname, UserOpts),
    AcctName = case binary_opt(name, Options) of
		   <<>> -> AdminUName;
		   Other -> Other
	       end,
    insert(Key, name, AcctName),
    exodm_db_user:new(AID, AdminUName, UserOpts),
    {ok, _RID} = t_create_role_(AID, AdminUName, initial_admin(Root)),
    UNameKey = to_binary(AdminUName),
    insert(exodm_db:join_key(Key, <<"admins">>), UNameKey, AdminUName),
    exodm_db_yang:init(AID),
    exodm_db_device:init(AID),
    exodm_db_config:init(AID),
    AIDVal = exodm_db:account_id_value(AID),

    %% lists:foreach(fun({I,Al}) ->
    %% 			  exodm_db:insert_alias(?TAB, Key, I, Al)
    %% 		  end, proplists:get_all_values(alias, Options)),
    %% add_user(AID, AdminUName, _RID = 1),  % must come after adding the user
    {ok, AIDVal}.

add_user(AID0, UID0) ->
    AID = exodm_db:account_id_key(AID0),
    UID = exodm_db:encode_id(UID0),
    kvdb_conf:write(
      table(), {kvdb_conf:join_key([AID,<<"users">>,UID]), [], <<>>}).

remove_user(AID0, UID0) ->
    AID = exodm_db:account_id_key(AID0),
    UID = exodm_db:encode_id(UID0),
    Tab = table(),
    exodm_db:in_transaction(
      fun(_) ->
	      AdminParent = kvdb_conf:join_key(AID, <<"admins">>),
	      case only_child(Tab, AdminParent, UID) of
		  true -> error(last_admin);
		  false ->
		      kvdb_conf:delete(
			Tab, kvdb_conf:join_key([AID, <<"users">>, UID])),
		      kvdb_conf:delete(
			Tab, kvdb_conf:join_key([AID, <<"admins">>, UID]))
	      end
      end).

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


initial_admin(true) ->
    [{descr, "initial root user"},
     {access, {<<"all">>, <<"root">>}}];
initial_admin(false) ->
    [{descr, "initial admin"},
     {access, {<<"all">>, <<"admin">>}}].

%% <AID>/roles/<Role>/descr
%%                   /groups/<gid> = access()
create_role(AID, UName, Opts) ->
    exodm_db:in_transaction(
      fun(_) ->
	      t_create_role(AID, UName, Opts)
      end).

t_create_role(AID0, UName, Opts) ->
    AID = exodm_db:account_id_key(AID0),
    check_access(AID),
    t_create_role_(AID, UName, Opts).

t_create_role_(AID, UName, Opts) ->
    case exodm_db_user:exist(UName) of
	true ->
	    {ok, RID} = create_role_(AID, UName, Opts),
	    exodm_db_user:add_access(AID, UName, RID),
	    {ok, RID};
	false ->
	    error(no_such_user)
    end.

create_role_(AID, UName, Opts) ->
    RID = new_role_id(AID),
    create_role_(AID, RID, UName, Opts).

create_role_(AID, RID, UName, Opts) ->
    Key = role_key(AID, RID),
    insert(Key, descr, binary_opt(descr, Opts)),
    insert(Key, uname, to_binary(UName)),
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
    {ok, exodm_db:role_id_num(RID)}.

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

%% role_exists(AID, Role0) ->
%%     Role = exodm_db:encode_id(to_binary(Role0)),
%%     exist(exodm_db:join_key([exodm_db:account_id_key(AID),
%% 				  <<"roles">>, Role, <<"descr">>])).

group_exists(AID, GID) ->
    case exodm_db:read(?TAB, exodm_db:join_key([AID, <<"groups">>,
						GID, <<"name">>])) of
	[] ->
	    false;
	_ ->
	    true
    end.

valid_access(<<"none">>) -> true;
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
	_ -> false;
	[_] -> false
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

%% delete the account, caller must make sure that data
%% access via this account is first deleted.
delete(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:in_transaction(
      fun(_Db) ->
	      kvdb_conf:delete_tree(table(), AID)
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

list_users(AID0, N, Prev) ->
    AID = exodm_db:account_id_key(AID0),
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
    FullPrev = kvdb_conf:join_key([AID, <<"admins">>, exodm_db:to_binary(Prev)]),
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
    kvdb_conf:fold_children(
      ?TAB, F, Acc, <<>>).

%% utils

key(AName) ->
    to_binary(AName).
%% exodm_db:join_key(<<"account">>, AName).

role_key(AID, RID) ->
    exodm_db:join_key([exodm_db:account_id_key(AID),
		       <<"roles">>,
		       exodm_db:role_id_key(RID)]).

new_role_id(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:update_counter(
      ?TAB,
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
    case UName of
	root -> AID;
	_ ->
	    case exodm_db:read(
		   ?TAB,exodm_db:join_key([AID, <<"admins">>,
					   exodm_db:encode_id(UName)])) of
		{ok, _}   -> AID;
		{error,_} -> error(not_authorized)
	    end
    end.

incr_request_id(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    kvdb_conf:update_counter(
      ?TAB, exodm_db:join_key(AID, <<"__last_req_id">>), 1).

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
      ?_test(?dbg(test_new()))
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
