%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense device manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_device).

-export([new/3, update/3, lookup/2, lookup/1, exist/2, exist/1]).
-export([key/2]).
-export([lookup_position/2, lookup_keys/2]).
-export([lookup_groups/2]).
-export([lookup_group_notifications/2]).

-import(exodm_db, [write/2, binary_opt/2, uint32_opt/2, to_binary/1]).
-import(lists, [reverse/1]).
%%
%% /<uid>/devices/<did>/name      = string()
%% /<uid>/devices/<did>/msisdn    = msisdn()
%% /<uid>/devices/<did>/imsi      = imsi()
%% /<uid>/devices/<did>/imei      = imei()
%% /<uid>/devices/<did>/longitud  = uint32()
%% /<uid>/devices/<did>/latitude  = uint32()
%% /<uid>/devices/<did>/timestamp = uint32()
%% /<uid>/devices/<did>/__ck      = uint64()
%% /<uid>/devices/<did>/__sk      = uint64()
%% /<uid>/devices/<did>/groups[<i>]/__gid = uint32()
%%

%% FIXME option validation

new(UID, DID, Options) ->
    Key = key(UID, DID),
    insert(Key,name,     binary_opt(name, Options)),
    insert(Key,msisdn,   binary_opt(msisdn,Options)),
    insert(Key,imsi,     binary_opt(imsi,Options)),
    insert(Key,imei,     binary_opt(imei,Options)),
    insert(Key,activity, uint32_opt(activity,Options)),
    insert(Key,longitude,uint32_opt(longitud,Options)),
    insert(Key,latitude, uint32_opt(latitude,Options)),
    insert(Key,timestamp,uint32_opt(timestamp,Options)),
    %% __ck, __sk are device keys and need special attention 
    insert(Key, '__ck',  binary_opt('__ck',Options)),
    insert(Key, '__sk',  binary_opt('__sk',Options)),
    lists:foreach(
      fun({I,GID}) ->
	      insert_group(Key, I, GID)
      end, proplists:get_all_values(group, Options)),
    ok.

%% FIXME validate every item BEFORE insert!
update(UID, DID, Options) ->
    Key = key(UID, DID),
    lists:foreach(
      fun
	  ({name,Value}) ->
	      insert(Key,name,to_binary(Value));
	  ({msisdn,Value}) ->
	      insert(Key,msisdn,to_binary(Value));
	  ({imsi,Value}) ->
	      insert(Key,imsi,to_binary(Value));
	  ({imei,Value}) ->
	      insert(Key,imei,to_binary(Value));
	  ({activity,Act}) ->
	      insert(Key,activity,<<Act:32>>);
	  ({latitude,Lat}) ->
	      insert(Key,latitude,<<Lat:32>>);
	  ({longitud,Lon}) ->
	      insert(Key,longitude,<<Lon:32>>);
	  ({timestamp,Ts}) ->
	      insert(Key,timestamp,<<Ts:32>>);
	  ({'__ck',Value}) when is_integer(Value) ->
	      insert(Key,'__ck', <<Value:64>>);
	  ({'__sk',Value}) when is_integer(Value) ->
	      insert(Key,'__sk', <<Value:64>>);
	  ({'__ck',Value}) when is_binary(Value), byte_size(Value) =:= 8 ->
	      insert(Key,'__ck', Value);
	  ({'__sk',Value}) when is_binary(Value), byte_size(Value) =:= 8 ->
	      insert(Key,'__sk', Value);
	  ({group,{I,GID}}) ->
	      insert_group(Key,I,GID)
      end, Options).

lookup(UID, DID) ->
    lookup(key(UID, DID)).

lookup(Key) ->
    <<_,ID/binary>> = lists:last(exodm_db:kvdb_key_split(Key)),
    [{id,ID}] ++
	read(Key,name) ++
	read(Key, msisdn) ++
	read(Key, imsi) ++
	read(Key, imei) ++
	read(Key, '__ck') ++
	read(Key, '__sk') ++
	read_uint32(Key, activity) ++
	read_uint32(Key, status) ++
	read_uint32(Key, longitude) ++
	read_uint32(Key, latitude) ++
	read_uint32(Key, timestamp).

lookup_groups(UID, DID) ->
    Key0 = key(UID, DID),
    exodm_db:fold_list(
      fun(I,Acc) ->
	      Key1 = exodm_db:kvdb_key_join(Key0, exodm_db:list_key(groups,I)),
	      case read_uint32(Key1, '__gid') of
		  [{_,GID}] -> [{group,{I,GID}} | Acc];
		  [] -> Acc
	      end
      end, [], Key0, groups).


lookup_group_notifications(UID, DID) ->
    lists:foldl(
      fun({group,{_I,GID}},Acc) ->
	      Props = exodm_db_group:lookup(UID, GID),
	      case proplists:get_value(url, Props, <<>>) of
		  <<>> -> Acc;
		  Url -> [Url | Acc]
		       end
      end,[],lookup_groups(UID, DID)).

%% find last known position or {0,0,0} if not found
lookup_position(UID, DID) ->
    Key = key(UID, DID),
    { 
      case read_uint32(Key,latitude) of
	  [] -> 0;
	  [{latitude,Lat}] -> Lat
      end,
      case read_uint32(Key,longitude) of
	  [] -> 0;
	  [{longitude,Lon}] -> Lon
      end,
      case read_uint32(Key,timestamp) of
	  [] -> 0;
	  [{timestamp,Ts}] -> Ts
      end
    }.

%% find last known position or {0,0,0} if not found
lookup_keys(UID, DID) ->
    Key = key(UID, DID),
    { 
      case read(Key,'__ck') of
	  [] -> <<0,0,0,0,0,0,0,0>>;
	  [{'__ck',Ck}] -> Ck
      end,
      case read(Key, '__sk') of
	  [] ->  <<0,0,0,0,0,0,0,0>>;
	  [{'__sk',Sk}] -> Sk
      end}.

exist(UID, DID) ->
    exist(key(UID, DID)).

exist(Key) ->
    case read(Key, name) of
	[] -> false;
	[_] -> true
    end.

%% utils

key(UID, DID) ->
    U = exodm_db:user_id_key(UID),
    D = exodm_db:device_id_key(DID),
    exodm_db:kvdb_key_join([U, <<"devices">>, D]).

insert(Key, Item, Value) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    exodm_db:write(Key1, Value).

insert_group(K0, I, GID) when 
      is_integer(I), I>=0, 
      is_integer(GID), GID >= 0 ->
    K = exodm_db:kvdb_key_join(K0, exodm_db:list_key(groups, I)),
    insert(K, '__gid',  <<GID:32>>).


read(Key,Item) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    case exodm_db:read(Key1) of
	{ok,{_,_,Value}} -> [{Item,Value}];
	{error,not_found} -> []
    end.

read_uint32(Key,Item) ->
    case read(Key,Item) of
	[{Item,<<Value:32>>}] -> [{Item,Value}];
	[] -> []
    end.

