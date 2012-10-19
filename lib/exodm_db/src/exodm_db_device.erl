%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense device manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_device).

-export([init/1]).
-export([new/3, update/3, lookup/2, lookup_attr/3,
	 delete/2, delete_devices/2,
	 add_config_set/3, list_config_sets/2,
	 yang_modules/2,
	 protocol/2,
	 exist/2]).
-export([list_device_keys/1, list_device_keys/2,
	 fold_devices/3, fold_devices/4]).
-export([key/2, tab_and_key/1]).
-export([enc_ext_key/2, dec_ext_key/1]).
-export([lookup_position/2, lookup_keys/2]).
-export([add_groups/3, lookup_groups/2]).
-export([lookup_group_notifications/2]).
-export([table/1]).
-export([client_auth_config/2]).

-include_lib("lager/include/log.hrl").

-import(exodm_db, [write/2, binary_opt/2, uint32_opt/2, to_binary/1]).
-import(lists, [reverse/1]).
%%
%% /<aid>/devices/<did>/name       = string()
%% /<aid>/devices/<did>/msisdn     = msisdn()
%% /<aid>/devices/<did>/imsi       = imsi()
%% /<aid>/devices/<did>/imei       = imei()
%% /<aid>/devices/<did>/longitud   = uint32()
%% /<aid>/devices/<did>/latitude   = uint32()
%% /<aid>/devices/<did>/timestamp  = uint32()
%% /<aid>/devices/<did>/device-key = uint64()
%% /<aid>/devices/<did>/did      = string()
%% /<aid>/devices/<did>/server-key = uint64()
%% /<aid>/devices/<did>/groups[<i>]/gid = uint32()
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
    insert_(Tab, AID, DID, Options).

insert_(Tab, AID, DID, Options) ->
    insert(Tab,DID, 'did',  exodm_db:decode_id(DID)),
    insert_keys(Tab, DID, Options),
    insert_groups(AID, Tab, DID, proplists:get_value(groups, Options, [])),
    insert_protocol(Tab, DID, proplists:get_value(protocol, Options)),
    _ = [insert_attr(Tab, DID, K,to_binary(V)) ||
	    {K,V} <- Options,
	    not lists:member(K, ['device-id','gid',
				 'server-key', 'device-key',
				 'protocol', 'groups'])],
    %% insert_attr(Tab,DID,name,     binary_opt(name, Options)),
    %% insert_attr(Tab,DID,description, binary_opt(description, Options)),
    %% insert(Tab,DID,msisdn,   binary_opt(msisdn,Options)),
    %% insert(Tab,DID,imsi,     binary_opt(imsi,Options)),
    %% insert(Tab,DID,imei,     binary_opt(imei,Options)),
    %% insert(Tab,DID,activity, uint32_opt(activity,Options)),
    %% insert(Tab,DID,longitude,uint32_opt(longitude,Options)),
    %% insert(Tab,DID,latitude, uint32_opt(latitude,Options)),
    %% insert(Tab,DID,timestamp,uint32_opt(timestamp,Options)),
    %% lists:foreach(
    %%   fun({I,GID}) ->
    %% 	      insert_group(Tab, Key, I, GID)
      %% end, proplists:get_all_values(group, Options)),
    ok.

delete_devices(AID, DevIdList) ->
    exodm_db:in_transaction(
      fun(_) ->
	      delete_devices_(AID, DevIdList)
      end).

delete_devices_(AID0, Devs) ->
    AID = exodm_db:account_id_key(AID0),
    [delete_device(AID, DID) || DID <- Devs].

delete_device(AID0, DID0) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    Tab = table(AID),
    kvdb_conf:delete_tree(Tab, DID),
    ConfigSets = list_config_sets(AID, DID),
    exodm_db_config:device_is_deleted(AID, DID, ConfigSets),
    Groups = lookup_groups(AID, DID),
    exodm_db_group:device_is_deleted(AID, DID, [G || {group,{_,G}} <- Groups]).

insert_keys(Tab,DID, Options) ->
    Lkup = fun(A,B) -> case proplists:get_value(A, Options) of
			   undefined ->
			       proplists:get_value(B, Options);
			   Val ->
			       Val
		       end
	   end,
    case Lkup('device-key', '__ck') of
	undefined ->  ok;
	DK -> insert(Tab,DID,'device-key', exodm_db:uint64_bin(DK))
    end,
    case Lkup('server-key', '__sk') of
	undefined ->  ok;
	SK -> insert(Tab,DID,'server-key', exodm_db:uint64_bin(SK))
    end.


%% FIXME validate every item BEFORE insert!
update(AID, DID, Options) ->
    update(AID, DID, _Delete = false, Options).

update(AID, DID, DeleteOther, Options) when is_boolean(DeleteOther) ->
    exodm_db:in_transaction(
      fun(_) ->
	      update_(AID, DID, DeleteOther, Options)
      end).

update_(AID0, DID0, DeleteOther, Options) ->
    DID = exodm_db:encode_id(DID0),
    AID = exodm_db:account_id_key(AID0),
    Tab = table(AID),
    case exist(AID, DID) of
	true ->
	    if DeleteOther ->
		    AttrsT = kvdb_conf:join_key(DID, <<"a">>),
		    kvdb_conf:delete_subtree(Tab, AttrsT);
	       true ->
		    ok
	    end,
	    insert_(Tab, AID, DID, Options)
    end.
    %% lists:foreach(
    %%   fun
    %% 	  ({name,Value}) ->
    %% 	      insert(Tab,DID,name,to_binary(Value));
    %% 	  ({msisdn,Value}) ->
    %% 	      insert(Tab,DID,msisdn, to_binary(Value));
    %% 	  ({imsi,Value}) ->
    %% 	      insert(Tab,DID,imsi,to_binary(Value));
    %% 	  ({imei,Value}) ->
    %% 	      insert(Tab,DID,imei,to_binary(Value));
    %% 	  ({activity,Act}) ->
    %% 	      insert(Tab,DID,activity,<<Act:32>>);
    %% 	  ({latitude,Lat}) ->
    %% 	      insert(Tab,DID,latitude,<<Lat:32>>);
    %% 	  ({longitude,Lon}) ->
    %% 	      insert(Tab,DID,longitude,<<Lon:32>>);
    %% 	  ({timestamp,Ts}) ->
    %% 	      insert(Tab,DID,timestamp,<<Ts:32>>);
    %% 	  ({group,{I,GID}}) ->
    %% 	      insert_group(AID,Tab,DID,I,GID);
    %% 	  ({protocol, Protocol0}) ->
    %% 	      Protocol = to_binary(Protocol0),
    %% 	      insert_protocol(Tab, DID, Protocol);
    %% 	  (_) ->
    %% 	      skip
    %%   end, Options),
    %% insert_keys(Tab, DID, Options).

insert_protocol(_Tab, _DID, undefined) ->
    error(unknown_protocol, [undefined]);
insert_protocol(Tab, DID, Protocol) ->
    case exodm_rpc_protocol:module(Protocol) of
	undefined ->
	    error(unknown_protocol, [Protocol]);
	_ ->
	    insert(Tab, DID, protocol, to_binary(Protocol))
    end.

delete(AID0, DID0) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    Tab = table(AID),
    exodm_db:in_transaction(
      fun(_) ->
	      case exist(AID, DID) of
		  true ->
		      Groups = lookup_groups(AID, DID),
		      kvdb_conf:delete_all(Tab, DID),
		      lists:foreach(
			fun({group,{_, GID}}) ->
				exodm_db_group:remove_device(AID, GID, DID)
			end, Groups);
		  false ->
		      ok
	      end
      end).

add_config_set(AID, DID, CfgName) ->
    exodm_db:in_transaction(fun(_) -> add_config_set_(AID, DID, CfgName) end).

add_config_set_(AID0, DID0, CfgName) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    Tab = table(AID),
    case exist(AID, DID) of
	true ->
	    insert(Tab, exodm_db:join_key(DID, <<"config_set">>),
		   CfgName, <<>>);
	false ->
	    error({unknown_device, [AID, DID]})
    end.


list_config_sets(AID0, DID0) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    Tab = table(AID),
    exodm_db:in_transaction(fun(_) -> list_config_sets_(Tab, DID) end).

list_config_sets_(Tab, DID) ->
    Set = kvdb_conf:fold_children(
	    Tab, fun(K, Acc) ->
			 [lists:last(exodm_db:split_key(K))|Acc]
		 end, [], exodm_db:join_key(DID, <<"config_set">>)),
    lists:reverse(Set).


yang_modules(AID0, DID0) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    Tab = table(AID),
    exodm_db:in_transaction(
      fun(_) ->
	      CDs = list_config_sets_(Tab, DID),
	      lists:map(
		fun(Name) ->
			{ok, Y} = exodm_db_config:get_yang_spec(AID, Name),
			URL = case exodm_db_config:get_url(AID, Name) of
				  {ok, U} -> U;
				  {error, not_found} -> <<>>
			      end,
			{Name, Y, URL}
		end, CDs)
      end).

protocol(AID0, DID0) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    Tab = table(AID),
    case read(Tab, DID, protocol) of
	[] ->
	    error(no_protocol_defined, [AID, DID]);
	[{_, P}] ->
	    P
    end.


lookup(AID, DID) ->
    lookup_(table(AID), exodm_db:encode_id(DID)).

lookup_(Tab,Key) ->
    [{id,Key}] ++
	read(Tab,Key,name) ++
	read(Tab,Key, msisdn) ++
	read(Tab,Key, imsi) ++
	read(Tab,Key, imei) ++
	read(Tab,Key, 'device-key') ++
	read(Tab,Key, 'server-key') ++
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
    lists:reverse(
      exodm_db:fold_list2(
	Tab,
	fun(I,Key,Acc) ->
		case read_uint32(Tab, Key, 'gid') of
		    [{_,GID}] -> [{group,{I,GID}} | Acc];
		    [] -> Acc
		end
	end, [], DID, groups)).


lookup_group_notifications(AID0, DID0) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    lists:foldl(
      fun({group,{_I,GID0}},Acc) ->
	      GID = exodm_db:group_id_key(GID0),
	      case exodm_db_group:lookup(AID, GID, url) of
		  [{_, URL}] ->
		      [URL | Acc];
		  [] ->
		      Acc
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
    case read(Tab, DID, 'device-key') of
	[] -> {error, not_found};
	[{_, Ck}] ->
	    case read(Tab, DID, 'server-key') of
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
      case read(Tab, DID,'device-key') of
	  [] -> <<0,0,0,0,0,0,0,0>>;
	  [{'device-key',Ck}] -> Ck
      end,
      case read(Tab, DID, 'server-key') of
	  [] ->  <<0,0,0,0,0,0,0,0>>;
	  [{'server-key',Sk}] -> Sk
      end}.

exist(AID, DID) ->
    case read(table(AID), exodm_db:encode_id(DID), did) of
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

insert_attr(Tab, Key, Item, Value) ->
    Key1 = exodm_db:join_key([Key, <<"a">>, to_binary(Item)]),
    exodm_db:write(Tab, Key1, Value).

insert_groups(AID, Tab, DID, Groups) ->
    insert_groups(AID, Tab, 1, DID, Groups).

insert_groups(AID, Tab, I0, DID, Groups0) ->
    {Groups,_} = lists:mapfoldl(fun(G,I) ->
					{{I,G}, I+1}
				end, I0, Groups0),
    lists:foreach(fun({I,G}) ->
			  insert_group(AID, Tab, DID, I, G)
		  end, Groups).

insert_group(AID, Tab, DID, I, GID) when is_integer(I) ->
    K = exodm_db:join_key(DID, exodm_db:list_key(groups, I)),
    exodm_db_group:add_device(AID, GID, DID),
    insert(Tab, K, 'gid',  gid_value(GID)).

add_groups(AID, DID0, Groups) ->
    DID = exodm_db:encode_id(DID0),
    Tab = table(AID),
    exodm_db:in_transaction(
      fun(_) ->
	      {Existing, Last} =
		  kvdb_conf:fold_list(
		    Tab, fun(I, Kl, {Acc,_}) ->
				 {ok,{_,_,<<GID:32>>}} =
				     kvdb_conf:read(
				       Tab, kvdb_conf:join_key(
					      Kl, <<"gid">>)),
				 case lists:member(GID, Groups) of
				     true ->
					 {[GID|Acc],I};
				     false ->
					 {Acc,I}
				 end
			 end, {[],0}, kvdb_conf:join_key(DID,<<"groups">>)),
	      ToAdd = Groups -- Existing,
	      insert_groups(AID, Tab, Last+1, DID, ToAdd)
      end).

remove_groups(AID, DID0, Groups) ->
    foo.

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
