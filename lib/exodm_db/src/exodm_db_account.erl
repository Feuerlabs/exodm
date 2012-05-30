%%% @author Ulf Wiger <ulf@feuerlabs.com>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense device manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_account).

-export([new/2, update/2, lookup/1, exist/1]).
-export([key/1]).
-import(exodm_db, [write/2, binary_opt/2, to_binary/1]).

%%
%% /<aid>/name  = <AccountName>
%% /<aid>/admin = <UID>
%%

%% FIXME option validation
new(Name, Options) ->
    case exist(Name) of
	true ->
	    {error, exists};
	false ->
	    AID = exodm_db_system:new_aid(),
	    Key = key(Name),
	    Admin = binary_opt(admin, Options),
	    insert(Key, name, Name),
	    insert(Key,'__aid', AID),
	    insert(Key,admin,     Admin),
	    exodm_db_system:set_aid_user(AID, Name),
	    exodm_db_yang:init(AID),
	    {ok, AID}
    end.

%% FIXME validate every item BEFORE insert!
update(AID, Options) ->
    case exodm_db_system:get_aid_user(AID) of
	{error, not_found} = E ->
	    E;
	Name0 ->
	    Key = key(AID),
	    Ops =
		lists:map(
		  fun
		      ({name,Value}) ->
			  case to_binary(Value) of
			      Name0 -> ok;
			      NewName ->
				  fun() ->
					  insert(Key,name,NewName),
					  exodm_db_system:set_aid_user(
					    AID, NewName)
				  end
			  end;
		      ({admin,Value}) ->
			  Admin = to_binary(Value),
			  case get_admin_uid(Admin) of
			      {error, not_found} ->
				  erlang:error({no_such_uid, Admin});
			      AUID ->
				  fun() ->
					  insert(Key,admin,AUID)
				  end
			  end
		  end, Options),
	    lists:foreach(fun(Op) -> Op() end, Ops)
    end.

lookup(AID) ->
    lookup_(key(AID)).

lookup_(Key) ->
    <<_,ID/binary>> = lists:last(exodm_db:kvdb_key_split(Key)),
    [{id,ID}] ++
	read(Key,name) ++
	read(Key, admin).

get_admin_uid(Admin) ->
    case exodm_db_system:get_uid_user(Admin) of
	{error, not_found} ->
	    case exodm_db_user:lookup_attr(Admin, <<"__uid">>) of
		{error, not_found} ->
		    {error, not_found};
		[UID] ->
		    UID
	    end;
	_ ->
	    Admin
    end.

exist(Acct) ->
    exist_(key(Acct)).

exist_(Key) ->
    case read(Key,name) of
	[] -> false;
	[_] -> true
    end.

%% utils

key(AName) ->
    exodm_db:kvdb_key_join(<<"account">>, AName).

insert(Key, Item, Value) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    exodm_db:write(Key1, Value).

read(Key,Item) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    case exodm_db:read(Key1) of
	{ok,{_,_,Value}} -> [{Item,Value}];
	{error,not_found} -> []
    end.
