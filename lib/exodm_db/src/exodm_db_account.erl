%%% @author Ulf Wiger <ulf@feuerlabs.com>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense device manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_account).

-export([new/1, update/2, lookup/1, exist/1]).
-export([list_account_keys/0, list_account_keys/1]).
-export([fold_accounts/2, fold_accounts/3]).
-export([list_users/1, list_users/2]).
-export([list_groups/1, list_groups/2]).
-export([list_admins/1]).
-export([key/1]).
-export([init/0]).
-import(exodm_db, [binary_opt/2, to_binary/1]).

-define(TAB, <<"acct">>).

init() ->
    exodm_db:add_table(?TAB, [alias]).

%%
%% /<aid>/name  = <AccountName>
%% /<aid>/admin = <UID>
%%

%% FIXME option validation
new(Options) ->
    %% initial check: there must be an admin
    UserOpts = case lists:keyfind(admin, 1, Options) of
		   false ->
		       error(no_admin_user);
		   {_, Os} when is_list(Os) ->
		       Os
	       end,
    Key = AID = exodm_db_system:new_aid(),
    Roles = initial_roles(proplists:get_all_values(roles, Options)),
    create_roles(AID, Roles),
    {_, AdminUName} = lists:keyfind(uname, 1, UserOpts),
    AcctName = case binary_opt(name, Options) of
		   <<>> -> AdminUName;
		   Other -> Other
	       end,
    %% AdminUName = exodm_db:encode_id(AdminUName0),
    insert(Key, name, AcctName),
    insert(exodm_db:kvdb_key_join(Key, exodm_db:list_key(admin, 1)),
	   '__uname', AdminUName),
    exodm_db_yang:init(AID),
    exodm_db_device:init(AID),
    exodm_db_user:new(AID, AdminUName, <<"admin">>, UserOpts),
    lists:foreach(fun({I,Al}) ->
			  exodm_db:insert_alias(?TAB, Key, I, Al)
		  end, proplists:get_all_values(alias, Options)),
    add_user(AID, AdminUName),  % must come after adding the user
    {ok, aid_value(AID)}.

initial_roles([]) ->
    [admin_role()];
initial_roles(Roles) when is_list(Roles) ->
    [admin_role() | lists:keydelete(<<"admin">>, 1, Roles)].

admin_role() ->
    { <<"admin">>, [ {access, {<<"all">>, <<"admin">>}} ] }.

create_roles(AID, Roles) ->
    lists:foreach(fun({Role, Opts}) ->
			  create_role(AID, Role, Opts)
		  end, Roles).

%% <AID>/roles/<Role>/descr
%%                   /groups/<gid> = access()
create_role(AID, Name0, Opts) ->
    Name = exodm_db:encode_id(to_binary(Name0)),
    Key = exodm_db:kvdb_key_join([AID, <<"roles">>, Name]),
    case exist(exodm_db:kvdb_key_join(Key, <<"descr">>)) of
	true ->
	    error({role_exists, Name0});
	false ->
	    insert(Key, descr, binary_opt(descr, Opts)),
	    GroupsKey = exodm_db:kvdb_key_join(Key, <<"groups">>),
	    lists:foreach(
	      fun({<<"all">>, Access}) ->
		      valid_access(Access),
		      insert(GroupsKey, <<"all">>, Access);
		 ({G, Access}) ->
		      valid_access(Access),
		      case group_exists(AID, G) of
			  true ->
			      insert(GroupsKey, G, Access);
			  false ->
			      error({no_such_group, [AID, G]})
		      end
	      end, proplists:get_all_values(access, Opts))
    end.

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

add_user(AID0, UName0) ->
    UName = exodm_db:encode_id(UName0),
    case exodm_db_user:exist(UName) of
	true ->
	    AID = exodm_db:account_id_key(AID0),
	    Key = exodm_db:kvdb_key_join(
		    [AID, <<"users">>, UName, <<"__uname">>]),
	    case exodm_db:read(?TAB, Key) of
		{error, not_found} ->
		    exodm_db:write(?TAB, Key, UName0),
		    ok;
		{ok, _} ->
		    {error, exists}
	    end;
	false ->
	    error(unknown_user, [AID0, UName0])
    end.

%% FIXME validate every item BEFORE insert!
update(AID0, Options) ->
    AID = exodm_db:admin_id_key(AID0),
    case exist(AID) of
	false ->
	    {error, not_found};
	true ->
	    update_(AID, Options)
    end.

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
    exodm_db:fold_list(
      ?TAB,
      fun(_, Key, Acc) ->
	      case read(Key, <<"__uname">>) of
		  [{_, UName}] ->
		      [UName|Acc];
		  [] ->
		      Acc
	      end
      end, [],
      exodm_db:kvdb_key_join(exodm_db:account_id_key(AID0), <<"admin">>)).

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

%% table(AID, users) ->
%%     exodm_db:kvdb_key_join(AID, <<"u">>);
%% table(AID, devices) ->
%%     exodm_db:kvdb_key_join(AID, <<"d">>).

insert(Key, Item, Value) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    exodm_db:write(?TAB, Key1, Value).

read(Key,Item) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    case exodm_db:read(?TAB, Key1) of
	{ok,{_,_,Value}} -> [{Item,Value}];
	{error,not_found} -> []
    end.

aid_value(AID) ->
    <<(exodm_db:account_id_num(AID)):32>>.
