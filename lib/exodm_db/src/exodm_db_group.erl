%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense device manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_group).

-export([new/2, update/3, lookup/2, lookup/1, exist/2, exist/1]).
-export([key/2]).
-import(exodm_db, [write/2, binary_opt/2, to_binary/1]).

%%
%% /<uid>/groups/<gid>/name  = <DeviceName>
%% /<uid>/groups/<gid>/url   = <MSISDN>
%%

%% FIXME option validation
new(AID, Options) ->
    new(AID, exodm_db_system:new_gid(), Options).

new(AID, GID, Options) ->
    case exodm_db_system:get_gid_user(GID) of
	{error, not_found} ->
	    Key = key(AID, GID),
	    Name = binary_opt(name, Options),
	    insert(Key,name,     Name),
	    insert(Key,url,      binary_opt(url,  Options)),
	    exodm_db_system:set_gid_user(GID, Name),
	    {ok, GID};
	_ ->
	    {error, exists}
    end.

%% FIXME validate every item BEFORE insert!
update(AID, GID, Options) ->
    case exodm_db_system:get_gid_user(GID) of
	{error, not_found} = E ->
	    E;
	Name0 ->
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

lookup(AID, GID) ->
    lookup(key(AID, GID)).

lookup(Key) ->
    <<_,ID/binary>> = lists:last(exodm_db:kvdb_key_split(Key)),
    [{id,ID}] ++
	read(Key,name) ++
	read(Key, url).

exist(AID, GID) ->
    exist(key(AID, GID)).

exist(Key) ->
    case read(Key,name) of
	[] -> false;
	[_] -> true
    end.

%% utils

key(AID, GID) ->
    A = exodm_db:account_id_key(AID),
    G = exodm_db:group_id_key(GID),
    exodm_db:kvdb_key_join([A, <<"groups">>, G]).

insert(Key, Item, Value) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    exodm_db:write(Key1, Value).

read(Key,Item) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    case exodm_db:read(Key1) of
	{ok,{_,_,Value}} -> [{Item,Value}];
	{error,not_found} -> []
    end.
