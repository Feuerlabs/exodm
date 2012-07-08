%%% @author Ulf Wiger <ulf@feuerlabs.com>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense device manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_account).

-export([new/1, update/2, lookup/1, exist/1]).
-export([create_role/3]).
-export([list_account_keys/0, list_account_keys/1]).
-export([fold_accounts/2, fold_accounts/3]).
-export([list_users/1, list_users/2]).
-export([list_groups/1, list_groups/2]).
-export([list_admins/1]).
-export([key/1, group_key/2, new_group_id/1,
	 table/0]).
-export([init/0]).
-import(exodm_db, [binary_opt/2, to_binary/1]).

-define(TAB, <<"acct">>).

init() ->
    exodm_db:in_transaction(
      fun(_) ->
	      exodm_db:add_table(?TAB, [alias])
      end).

table() ->
    ?TAB.

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
    Key = AID = exodm_db_system:new_aid(),
    {_, AdminUName} = lists:keyfind(uname, 1, UserOpts),
    {ok, RID} = create_role_(AID, 1, AdminUName, initial_admin()),
    AcctName = case binary_opt(name, Options) of
		   <<>> -> AdminUName;
		   Other -> Other
	       end,
    insert(Key, '__last_rid', <<1:32>>), % one role already created
    insert(Key, '__last_gid', <<0:32>>),
    insert(Key, name, AcctName),
    UNameKey = exodm_db:encode_id(AdminUName),
    insert(exodm_db:kvdb_key_join(Key, <<"admins">>), UNameKey, AdminUName),
    exodm_db_yang:init(AID),
    exodm_db_device:init(AID),
    exodm_db_user:new(AID, AdminUName, exodm_db:role_id_value(RID), UserOpts),
    %% lists:foreach(fun({I,Al}) ->
    %% 			  exodm_db:insert_alias(?TAB, Key, I, Al)
    %% 		  end, proplists:get_all_values(alias, Options)),
    %% add_user(AID, AdminUName, _RID = 1),  % must come after adding the user
    {ok, exodm_db:account_id_value(AID)}.

initial_admin() ->
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
    AID = check_access(AID0),
    case exodm_db_user:exist(UName) of
	true ->
	    {ok, RID} = create_role_(AID, UName, Opts),
	    exodm_db_user:add_access(AID, UName, RID);
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
    AccessKey = exodm_db:kvdb_key_join(Key, <<"access">>),
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


%% role_exists(AID, Role0) ->
%%     Role = exodm_db:encode_id(to_binary(Role0)),
%%     exist(exodm_db:kvdb_key_join([exodm_db:account_id_key(AID),
%% 				  <<"roles">>, Role, <<"descr">>])).

group_exists(AID, GID) ->
    case exodm_db:read(?TAB, exodm_db:kvdb_key_join([AID, <<"groups">>,
						     GID, <<"name">>])) of
	[] ->
	    false;
	_ ->
	    true
    end.

valid_access(A) ->
    case lists:member(A, [<<"none">>, <<"admin">>, <<"config">>, <<"exec">>]) of
	true -> true;
	false -> error({invalid_access, A})
    end.

%% add_user(AID0, UName, RID) ->
%%     AID = check_access(AID0),
%%     add_user_(AID, UName, RID).

%% add_user_(AID0, UName0, RID) ->
%%     UName = exodm_db:encode_id(UName0),
%%     case exodm_db_user:exist(UName) of
%% 	true ->
%% 	    AID = exodm_db:account_id_key(AID0),
%% 	    Key = exodm_db:kvdb_key_join(
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
    Ops =
	lists:map(
	  fun
	      ({name,Value}) ->
		  NewName = to_binary(Value),
		  fun() ->
			  insert(Key, name, NewName)
		  end;
	      ({add_admin,UName}) ->
		  case exodm_db_user:exist(UName) of
		      false ->
			  error({no_such_user, UName});
		      true ->
			  Last = exodm_db:fold_list(
				   ?TAB,
				   fun(I, LKey, _Acc) ->
					   case exist(exodm_db:kvdb_key_join(
							LKey, <<"__uname">>)) of
					       true ->
						   error({admin_exists, UName});
					       false ->
						   I
					   end
				   end, 0,
				   exodm_db:kvdb_key_join(AID, <<"admin">>)),
			  fun() ->
				  insert(
				    exodm_db:kvdb_key_join(
				      AID, exodm_db:list_key(admin, Last+1)),
				      '__uname', UName)
			  end
		  end
	  end, Options),
    lists:foreach(fun(Op) -> Op() end, Ops).

lookup(AID) ->
    lookup_(key(AID)).

lookup_(Key) ->
    <<_,ID/binary>> = lists:last(exodm_db:kvdb_key_split(Key)),
    [{id,ID}] ++
	read(Key,name) ++
	read(Key, admin).

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

exist(Acct) ->
    exist_(key(Acct)).

exist_(Key) ->
    case read(Key,name) of
	[] -> false;
	[_] -> true
    end.

list_account_keys() ->
    list_account_keys(30).

list_account_keys(Limit) ->
    fold_accounts(fun(AID, Acc) ->
			  [AID|Acc]
		  end, [], Limit).

list_users(AID) ->
    list_users(AID, 30).

list_users(AID0, Limit) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:fold_keys(
      <<"data">>,
      exodm_db:kvdb_key_join(AID, <<"users">>),
      fun([A, <<"users">>, UID|_], Acc) ->
	      {next, exodm_db:kvdb_key_join([A,<<"users">>, UID]), [UID|Acc]};
	 (_, Acc) ->
	      {done, Acc}
      end, [], Limit).

list_groups(AID) ->
    list_groups(AID, 30).

list_groups(AID, Limit) ->
    exodm_db_groups:list_group_keys(AID, Limit).

list_admins(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:fold_keys(
      ?TAB,
      exodm_db:kvdb_key_join(AID, <<"admins">>),
      fun([UName|_], Acc) ->
	      [UName|Acc]
      end, [], 100).

fold_accounts(F, Acc) ->
    fold_accounts(F, Acc, 30).

fold_accounts(F, Acc, Limit) when
      Limit==infinity; is_integer(Limit), Limit > 0 ->
    exodm_db:fold_keys(
      ?TAB,
      <<>>,
      fun([AID|_], Acc1) ->
	      {next, AID, F(AID,Acc1)};
	 (_, Acc1) ->
	      {done, Acc1}
      end, Acc, Limit).

%% utils

key(AName) ->
    to_binary(AName).
    %% exodm_db:kvdb_key_join(<<"account">>, AName).

group_key(AID, GID) ->
    exodm_db:kvdb_key_join([exodm_db:account_id_key(AID),
			    <<"groups">>,
			    exodm_db:group_id_key(GID)]).

new_group_id(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:update_counter(
      ?TAB, exodm_db:kvdb_key_join(AID, <<"__last_gid">>), 1).

role_key(AID, RID) ->
    exodm_db:kvdb_key_join([exodm_db:account_id_key(AID),
			    <<"roles">>,
			    exodm_db:role_id_key(RID)]).

new_role_id(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:update_counter(
      exodm_db:kvdb_key_join(AID, <<"__last_rid">>), 1).

insert(Key, Item, Value) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    exodm_db:write(?TAB, Key1, Value).

read(Key,Item) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    case exodm_db:read(?TAB, Key1) of
	{ok,{_,_,Value}} -> [{Item,Value}];
	{error,not_found} -> []
    end.

check_access(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    UName = exodm_db_session:get_user(),
    case UName of
	superuser -> AID;
	_ ->
	    case exodm_db:read(
		   ?TAB,exodm_db:kvdb_key_join([AID, <<"admins">>,
						exodm_db:encode_id(UName)])) of
		{ok, _}   -> AID;
		{error,_} -> error(not_authorized)
	    end
    end.
