%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense device manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_group).

-export([new/2, update/3, lookup/2, lookup/1, exist/2, exist/1,
	 delete/2]).
-export([list_group_keys/1, list_group_keys/2]).
-export([fold_groups/3, fold_groups/4]).
-export([key/2, tab_and_key/1]).
-import(exodm_db, [write/2, binary_opt/2, to_binary/1]).

%%
%% /<aid>/groups/<gid>/name  = <DeviceName>
%% /<aid>/groups/<gid>/url   = <MSISDN>
%%

%% FIXME option validation
new(AID, Options) ->
    new(AID, exodm_db_account:new_group_id(AID), Options).

new(AID, GID, Options) ->
    Key = key(AID, GID),
    case read(Key, name) of
	[] ->
	    Name = binary_opt(name, Options),
	    insert(Key,name,     Name),
	    insert(Key,url,      binary_opt(url,  Options)),
	    {ok, gid_value(GID)};
	[_] ->
	    {error, exists}
    end.

%% FIXME validate every item BEFORE insert!
update(AID, GID, Options) ->
    Key = key(AID, GID),
    case read(Key, name) of
	[] ->
	    {error, not_found};
	[{_, Name0}] ->
	    Key = key(AID, GID),
	    lists:foreach(
	      fun
		  ({name,Value}) ->
		      case to_binary(Value) of
			  Name0 -> ok;
			  NewName ->
			      insert(Key,name,NewName),
			      exodm_db_system:set_gid_user(GID, NewName)
		      end;
		  ({url,Value}) ->
		      insert(Key,url,to_binary(Value))
	      end, Options)
    end.

delete(AID, GID) ->
    Key = key(AID, GID),
    case exist(Key) of
	true ->
	    kvdb_conf:delete_all(table(), Key);
	false ->
	    {error, not_found}
    end.

lookup(AID, GID) ->
    lookup(key(AID, GID)).

lookup(Key) ->
    GID = lists:last(exodm_db:kvdb_key_split(Key)),
    [{id,gid_value(GID)}] ++
	read(Key,name) ++
	read(Key, url).

exist(AID, GID) ->
    exist(key(AID, GID)).

exist(Key) ->
    case read(Key,name) of
	[] -> false;
	[_] -> true
    end.

list_group_keys(AID) ->
    list_group_keys(AID, 30).

list_group_keys(AID, Limit) ->
    fold_groups(fun(GID, Acc) ->
			[GID|Acc]
		end, [], AID, Limit).

fold_groups(F, Acc, AID) ->
    fold_groups(F, Acc, AID, 30).

fold_groups(F, Acc, AID0, Limit) when
      Limit==infinity; is_integer(Limit), Limit > 0 ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:fold_keys(
      table(),
      exodm_db:kvdb_key_join(AID, <<"groups">>),
      fun([_AID,<<"groups">>,GID|_], Acc1) ->
	      {next, GID, F(GID,Acc1)}
      end, Acc, Limit).

%% utils

table() ->
    exodm_db_account:table().

key(AID, GID) ->
    exodm_db_account:group_key(AID, GID).

tab_and_key(AID) ->
    {table(),
     exodm_db:kvdb_key_join(exodm_db:account_id_key(AID), <<"groups">>)}.

insert(Key, Item, Value) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    exodm_db:write(exodm_db_account:table(), Key1, Value).

read(Key,Item) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    case exodm_db:read(exodm_db_account:table(), Key1) of
	{ok,{_,_,Value}} -> [{Item,Value}];
	{error,not_found} -> []
    end.

gid_value(GID) ->
    <<(exodm_db:group_id_num(GID)):32>>.
