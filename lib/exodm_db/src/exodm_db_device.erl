%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense device manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_device).

-export([new/2, update/3, lookup/2, lookup/1, lookup_attr/3,
	 exist/2, exist/1]).
-export([key/2]).
-export([enc_ext_key/2, dec_ext_key/1]).
-export([lookup_position/2, lookup_keys/2]).
-export([lookup_groups/2]).
-export([lookup_group_notifications/2]).
-export([mk_msisdn/2]).

-export([client_auth_config/2]).

-include_lib("lager/include/log.hrl").

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

new(AID, Options) ->
    ?info("new(~p, ~p)~n", [AID, Options]),
    DID = exodm_db_system:new_did(),
    Key = key(AID, DID),
    insert(Key,name,     binary_opt(name, Options)),
    insert(Key,msisdn,   mk_msisdn(binary_opt(msisdn,Options), DID)),
    insert(Key,imsi,     binary_opt(imsi,Options)),
    insert(Key,imei,     binary_opt(imei,Options)),
    insert(Key,activity, uint32_opt(activity,Options)),
    insert(Key,longitude,uint32_opt(longitud,Options)),
    insert(Key,latitude, uint32_opt(latitude,Options)),
    insert(Key,timestamp,uint32_opt(timestamp,Options)),
    insert(Key,yang, binary_opt(yang, Options)),
    %% __ck, __sk are device keys and need special attention 
    insert(Key, '__ck',  binary_opt('__ck',Options)),
    insert(Key, '__sk',  binary_opt('__sk',Options)),
    lists:foreach(
      fun({I,GID}) ->
	      insert_group(Key, I, GID)
      end, proplists:get_all_values(group, Options)),
    ok.

%% FIXME validate every item BEFORE insert!
update(AID, DID, Options) ->
    Key = key(AID, DID),
    lists:foreach(
      fun
	  ({name,Value}) ->
	      insert(Key,name,to_binary(Value));
	  ({msisdn,Value}) ->
	      insert(Key,msisdn,mk_msisdn(to_binary(Value), DID));
	  ({imsi,Value}) ->
	      insert(Key,imsi,to_binary(Value));
	  ({imei,Value}) ->
	      insert(Key,imei,to_binary(Value));
	  ({activity,Act}) ->
	      insert(Key,activity,<<Act:32>>);
	  ({latitude,Lat}) ->
	      insert(Key,latitude,<<Lat:32>>);
	  ({longitude,Lon}) ->
	      insert(Key,longitude,<<Lon:32>>);
	  ({timestamp,Ts}) ->
	      insert(Key,timestamp,<<Ts:32>>);
	  ({'__ck',Value}) when is_binary(Value), byte_size(Value) =:= 8 ->
	      insert(Key,'__ck', Value);
	  ({'__sk',Value}) when is_binary(Value), byte_size(Value) =:= 8 ->
	      insert(Key,'__sk', Value);
	  ({group,{I,GID}}) ->
	      insert_group(Key,I,GID);
	  ({yang,Value}) ->
	      insert(Key, yang, to_binary(Value))
      end, Options).

lookup(AID, DID) ->
    lookup(key(AID, DID)).

lookup(Key) ->
    <<_,ID/binary>> = lists:last(exodm_db:kvdb_key_split(Key)),
    [{id,ID}] ++
	read(Key,name) ++
	read(Key, msisdn) ++
	read(Key, imsi) ++
	read(Key, imei) ++
	read(Key, '__ck') ++
	read(Key, '__sk') ++
	read(Key, yang) ++
	read_uint32(Key, activity) ++
	read_uint32(Key, status) ++
	read_uint32(Key, longitude) ++
	read_uint32(Key, latitude) ++
	read_uint32(Key, timestamp).

lookup_attr(AID, DID, Attr) ->
    Key = key(AID, DID),
    read(Key, Attr).

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

dec_ext_key(<<$a, Ia:8/binary, $x, Ix:8/binary>>) ->
    {<<"a", Ia/binary>>, <<"x", Ix/binary>>};
dec_ext_key(_) ->
    error.

enc_ext_key(<<$a,_/binary>> = AID, <<$x, _/binary>> = DID) ->
    <<AID/binary, DID/binary>>;
enc_ext_key(AID, DID) ->
    enc_ext_key(exodm_db:account_id_key(AID), exodm_db:device_id_key(DID)).


%% @doc Convenience function to extract a client auth config for the
%% device BERT RPC client.
client_auth_config(AID0, DID0) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:device_id_key(DID0),
    Key = key(AID, DID),
    case read(Key, '__ck') of
	[] -> {error, not_found};
	[{_, Ck}] ->
	    case read(Key, '__sk') of
		[] -> {error, not_found};
		[{_, Cs}] ->
		    ExtKey = enc_ext_key(AID, DID),
		    {client, {ExtKey, Ck, Cs}}
	    end
    end.


mk_msisdn(<<>>, _) ->
    <<>>;
mk_msisdn(Bin, <<$x, ID/binary>>) ->
    DID_len = 19 - byte_size(Bin),  % 15 + length("$DID") -> 19
    N = pad(integer_to_list(list_to_integer(binary_to_list(ID), 16)), DID_len),
    re:replace(Bin, "\\$DID", N, [{return, binary}]).

pad(S, N) ->
    lists:duplicate(N - length(S), $0) ++ S.


%% find last known position or {0,0,0} if not found
lookup_keys(AID, DID) ->
    Key = key(AID, DID),
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

key(AID, DID) ->
    A = exodm_db:account_id_key(AID),
    D = exodm_db:device_id_key(DID),
    exodm_db:kvdb_key_join([A, <<"devices">>, D]).

insert(Key, Item, Value) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    exodm_db:write(Key1, Value).

insert_group(K0, I, GID0) when is_integer(I), I >= 0 ->
    GID = exodm_db:group_id_key(GID0),
    K = exodm_db:kvdb_key_join(K0, exodm_db:list_key(groups, I)),
    insert(K, '__gid',  GID).

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

