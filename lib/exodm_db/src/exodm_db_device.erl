%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense device manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_device).

-export([init/1]).
-export([new/3, update/3, lookup/2, lookup_attr/3,
	 add_config_data/3, list_config_data/2,
	 yang_modules/2,
	 exist/2]).
-export([list_device_keys/1, list_device_keys/2,
	 fold_devices/3, fold_devices/4]).
-export([key/2, tab_and_key/1]).
-export([enc_ext_key/2, dec_ext_key/1]).
-export([lookup_position/2, lookup_keys/2]).
-export([lookup_groups/2]).
-export([lookup_group_notifications/2]).
-export([table/1]).
-export([client_auth_config/2]).

-include_lib("lager/include/log.hrl").

-import(exodm_db, [write/2, binary_opt/2, uint32_opt/2, to_binary/1]).
-import(lists, [reverse/1]).
%%
%% /<aid>/devices/<did>/name      = string()
%% /<aid>/devices/<did>/msisdn    = msisdn()
%% /<aid>/devices/<did>/imsi      = imsi()
%% /<aid>/devices/<did>/imei      = imei()
%% /<aid>/devices/<did>/longitud  = uint32()
%% /<aid>/devices/<did>/latitude  = uint32()
%% /<aid>/devices/<did>/timestamp = uint32()
%% /<aid>/devices/<did>/__ck      = uint64()
%% /<aid>/devices/<did>/__did     = string()
%% /<aid>/devices/<did>/__sk      = uint64()
%% /<aid>/devices/<did>/groups[<i>]/__gid = uint32()
%%

%% FIXME option validation

init(AID) ->
    exodm_db:in_transaction(
      fun(_) ->
	      kvdb_conf:add_table(table(AID), [])
      end).

table(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    <<AID/binary, "_dev">>.

new(AID, ID, Options) ->
    exodm_db:in_transaction(
      fun(_) ->
	      new_(AID, ID, Options)
      end).

new_(AID0, ID0, Options) ->
    ?info("new(~p, ~p, ~p)~n", [AID0, ID0, Options]),
%%    DID = exodm_db_system:new_did(),
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(ID0),
    Tab = table(AID),
    Key = DID,
    insert(Tab,Key,'__did',  to_binary(ID0)),
    insert(Tab,Key,name,     binary_opt(name, Options)),
    insert(Tab,Key,msisdn,   binary_opt(msisdn,Options)),
    insert(Tab,Key,imsi,     binary_opt(imsi,Options)),
    insert(Tab,Key,imei,     binary_opt(imei,Options)),
    insert(Tab,Key,activity, uint32_opt(activity,Options)),
    insert(Tab,Key,longitude,uint32_opt(longitud,Options)),
    insert(Tab,Key,latitude, uint32_opt(latitude,Options)),
    insert(Tab,Key,timestamp,uint32_opt(timestamp,Options)),
    insert(Tab,Key,yang, binary_opt(yang, Options)),
    %% __ck, __sk are device keys and need special attention 
    insert(Tab,Key, '__ck',  binary_opt('__ck',Options)),
    insert(Tab,Key, '__sk',  binary_opt('__sk',Options)),
    insert_groups(Tab, Key, proplists:get_value(groups, Options, [])),
    %% lists:foreach(
    %%   fun({I,GID}) ->
    %% 	      insert_group(Tab, Key, I, GID)
      %% end, proplists:get_all_values(group, Options)),
    ok.

%% FIXME validate every item BEFORE insert!
update(AID, DID, Options) ->
    exodm_db:in_transaction(
      fun(_) ->
	      update_(AID, DID, Options)
      end).

update_(AID0, DID0, Options) ->
    DID = exodm_db:encode_id(DID0),
    AID = exodm_db:account_id_key(AID0),
    Tab = table(AID),
    lists:foreach(
      fun
	  ({name,Value}) ->
	      insert(Tab,DID,name,to_binary(Value));
	  ({msisdn,Value}) ->
	      insert(Tab,DID,msisdn, to_binary(Value));
	  ({imsi,Value}) ->
	      insert(Tab,DID,imsi,to_binary(Value));
	  ({imei,Value}) ->
	      insert(Tab,DID,imei,to_binary(Value));
	  ({activity,Act}) ->
	      insert(Tab,DID,activity,<<Act:32>>);
	  ({latitude,Lat}) ->
	      insert(Tab,DID,latitude,<<Lat:32>>);
	  ({longitude,Lon}) ->
	      insert(Tab,DID,longitude,<<Lon:32>>);
	  ({timestamp,Ts}) ->
	      insert(Tab,DID,timestamp,<<Ts:32>>);
	  ({'__ck',Value}) when is_binary(Value), byte_size(Value) =:= 8 ->
	      insert(Tab,DID,'__ck', Value);
	  ({'__sk',Value}) when is_binary(Value), byte_size(Value) =:= 8 ->
	      insert(Tab,DID,'__sk', Value);
	  ({group,{I,GID}}) ->
	      insert_group(Tab,DID,I,GID);
	  ({yang,Value}) ->
	      insert(Tab,DID, yang, to_binary(Value))
      end, Options).

add_config_data(AID, DID, CfgName) ->
    exodm_db:in_transaction(fun(_) -> add_config_data_(AID, DID, CfgName) end).

add_config_data_(AID0, DID0, CfgName) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    Tab = table(AID),
    case exist(AID, DID) of
	true ->
	    insert(Tab, exodm_db:join_key(DID, <<"config_data">>),
		   CfgName, <<>>);
	false ->
	    error({unknown_device, [AID, DID]})
    end.


list_config_data(AID0, DID0) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    Tab = table(AID),
    exodm_db:in_transaction(fun(_) -> list_config_data_(Tab, DID) end).

list_config_data_(Tab, DID) ->
    Set = kvdb_conf:fold_children(
	    Tab, fun(K, Acc) ->
			 [lists:last(exodm_db:split_key(K))|Acc]
		 end, [], exodm_db:join_key(DID, <<"config_data">>)),
    lists:reverse(Set).


yang_modules(AID0, DID0) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    Tab = table(AID),
    exodm_db:in_transaction(
      fun(_) ->
	      CDs = list_config_data_(Tab, DID),
	      [{Name,
		exodm_db_config:get_yang_spec(AID, Name),
		exodm_db_config:get_protocol(AID, Name)} || Name <- CDs]
      end).

lookup(AID, DID) ->
    lookup_(table(AID), exodm_db:encode_id(DID)).

lookup_(Tab,Key) ->
    [{id,Key}] ++
	read(Tab,Key,name) ++
	read(Tab,Key, msisdn) ++
	read(Tab,Key, imsi) ++
	read(Tab,Key, imei) ++
	read(Tab,Key, '__ck') ++
	read(Tab,Key, '__sk') ++
	read(Tab,Key, yang) ++
	read_uint32(Tab,Key, activity) ++
	read_uint32(Tab,Key, status) ++
	read_uint32(Tab,Key, longitude) ++
	read_uint32(Tab,Key, latitude) ++
	read_uint32(Tab,Key, timestamp).

lookup_attr(AID, DID, Attr) ->
    read(table(AID), exodm_db:encode_id(DID), Attr).

list_device_keys(AID) ->
    list_device_keys(AID, 30).

list_device_keys(AID, Limit) ->
    fold_devices(fun(DID, Acc) ->
			 [DID|Acc]
		 end, [], AID, Limit).

fold_devices(F, Acc, AID) ->
    fold_devices(F, Acc, AID, 30).

fold_devices(F, Acc, AID, Limit) when
      Limit==infinity; is_integer(Limit), Limit > 0 ->
    Tab = table(AID),
    exodm_db:fold_keys(
      Tab,
      <<>>,
      fun([DID|_], Acc1) ->
	      {next, DID, F(DID,Acc1)}
      end, Acc, Limit).


lookup_groups(AID, DID0) ->
    Tab = table(AID),
    DID = exodm_db:encode_id(DID0),
    exodm_db:fold_list2(
      Tab,
      fun(I,Key,Acc) ->
	      case read_uint32(Tab, Key, '__gid') of
		  [{_,GID}] -> [{group,{I,GID}} | Acc];
		  [] -> Acc
	      end
      end, [], DID, groups).


lookup_group_notifications(AID, DID) ->
    lists:foldl(
      fun({group,{_I,GID}},Acc) ->
	      Props = exodm_db_group:lookup(AID, GID),
	      case proplists:get_value(url, Props, <<>>) of
		  <<>> -> Acc;
		  Url -> [Url | Acc]
		       end
      end,[],lookup_groups(AID, DID)).

%% find last known position or {0,0,0} if not found
lookup_position(AID, DID0) ->
    Tab = table(AID),
    DID = exodm_db:encode_id(DID0),
    {
      case read_uint32(Tab,DID,latitude) of
	  [] -> 0;
	  [{latitude,Lat}] -> Lat
      end,
      case read_uint32(Tab,DID,longitude) of
	  [] -> 0;
	  [{longitude,Lon}] -> Lon
      end,
      case read_uint32(Tab,DID,timestamp) of
	  [] -> 0;
	  [{timestamp,Ts}] -> Ts
      end
    }.

dec_ext_key(<<$a, Ia:8/binary, "=", Ix/binary>>) ->
    {<<"a", Ia/binary>>, <<"=", Ix/binary>>};
dec_ext_key(Key) ->
    try
	AID = exodm_db_session:get_aid(),
	{exodm_db:account_id_key(AID), exodm_db:encode_id(Key)}
    catch
	error:_ ->
	    error
    end.


enc_ext_key(<<$a,_/binary>> = AID, <<$=, _/binary>> = DID) ->
    <<AID/binary, DID/binary>>;
enc_ext_key(AID, DID) ->
    enc_ext_key(exodm_db:account_id_key(AID), exodm_db:encode_id(DID)).


%% @doc Convenience function to extract a client auth config for the
%% device BERT RPC client.
client_auth_config(AID0, DID0) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    Tab = table(AID),
    case read(Tab, DID, '__ck') of
	[] -> {error, not_found};
	[{_, Ck}] ->
	    case read(Tab, DID, '__sk') of
		[] -> {error, not_found};
		[{_, Cs}] ->
		    ExtKey = enc_ext_key(AID, DID),
		    [{bert,
		      [{auth, [
			       {client, [{id, ExtKey},
					 {keys, {Ck, Cs}},
					 {mod, bert_challenge}]}
			      ]},
		       {reuse_mode, client}]}]
	    end
    end.



%% find last known position or {0,0,0} if not found
lookup_keys(AID, DID0) ->
    DID = exodm_db:encode_id(DID0),
    Tab = table(AID),
    {
      case read(Tab, DID,'__ck') of
	  [] -> <<0,0,0,0,0,0,0,0>>;
	  [{'__ck',Ck}] -> Ck
      end,
      case read(Tab, DID, '__sk') of
	  [] ->  <<0,0,0,0,0,0,0,0>>;
	  [{'__sk',Sk}] -> Sk
      end}.

exist(AID, DID) ->
    case read(table(AID), exodm_db:encode_id(DID), name) of
	[] -> false;
	[_] -> true
    end.

%% utils

key(AID, DID) ->
    A = exodm_db:account_id_key(AID),
    D = exodm_db:encode_id(DID),
    exodm_db:join_key([A, <<"devices">>, D]).

tab_and_key(AID) ->
    {table(AID), <<>>}.

insert(Tab, Key, Item, Value) ->
    Key1 = exodm_db:join_key([Key, to_binary(Item)]),
    exodm_db:write(Tab, Key1, Value).

insert_groups(Tab, K, Groups0) ->
    {Groups,_} = lists:mapfoldl(fun(G,I) ->
					{{I,G}, I+1}
				end, 1, Groups0),
    lists:foreach(fun({I,G}) ->
			  insert_group(Tab, K, I, G)
		  end, Groups).

insert_group(Tab, K0, I, GID) when is_integer(I) ->
    K = exodm_db:join_key(K0, exodm_db:list_key(groups, I)),
    insert(Tab, K, '__gid',  gid_value(GID)).

gid_value(GID) ->
    <<(exodm_db:group_id_num(GID)):32>>.

read(Tab, Key,Item) ->
    Key1 = exodm_db:join_key([Key, to_binary(Item)]),
    case exodm_db:read(Tab, Key1) of
	{ok,{_,_,Value}} -> [{Item,Value}];
	{error,not_found} -> []
    end.

read_uint32(Tab, Key,Item) ->
    case read(Tab, Key,Item) of
	[{Item,<<Value:32>>}] -> [{Item,Value}];
	[] -> []
    end.

