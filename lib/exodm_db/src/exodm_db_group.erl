%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense device manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_group).

-export([new/3, update/3, lookup/2, lookup/1, exist/2, exist/1]).

-import(exodm_db, [write/2, binary_opt/2, to_binary/1]).

%%
%% /<uid>/groups/<gid>/name  = <DeviceName>
%% /<uid>/groups/<gid>/url   = <MSISDN>
%%

%% FIXME option validation

new(UID, GID, Options) ->
    Key = key(UID, GID),
    insert(Key,name,     binary_opt(name, Options)),
    insert(Key,url,      binary_opt(url,  Options)),
    ok.

%% FIXME validate every item BEFORE insert!
update(UID, GID, Options) ->
    Key = key(UID, GID),
    lists:foreach(
      fun
	  ({name,Value}) ->
	      insert(Key,name,to_binary(Value));
	  ({url,Value}) ->
	      insert(Key,url,to_binary(Value))
      end, Options).

lookup(UID, GID) ->
    lookup(key(UID, GID)).

lookup(Key) ->
    <<_,ID/binary>> = lists:last(exodm_db:kvdb_key_split(Key)),
    [{id,ID}] ++
	read(Key,name) ++
	read(Key, url).

exist(UID, GID) ->
    exist(key(UID, GID)).

exist(Key) ->
    case exodm_db:read(Key) of
	{ok, _} -> true;
	{error, not_found} -> false
    end.


%% utils

key(UID, GID) ->
    U = exodm_db:user_id_key(UID),
    G = exodm_db:group_id_key(GID),
    exodm_db:kvdb_key_join([U, <<"groups">>, G]).

insert(Key, Item, Value) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    exodm_db:write(Key1, Value).

read(Key,Item) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    case exodm_db:read(Key1) of
	{ok,{_,_,Value}} -> [{Item,Value}];
	{error,not_found} -> []
    end.
