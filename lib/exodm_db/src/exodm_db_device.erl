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
-export([add_groups/3, lookup_groups/2, remove_groups/3]).
-export([lookup_group_notifications/2]).
-export([table/1]).
-export([client_auth_config/2]).

-export([code_change/2]).

-include_lib("lager/include/log.hrl").
-include_lib("kvdb/include/kvdb_conf.hrl").

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
    exodm_db:all_required([protocol, 'device-type'], Options),
    Tab = table(AID),
    insert_(Tab, AID, DID, Options).

%% Called at both new() and update(). Device-type and protocol are not required
%% at update, but presence verified in new_/3 above. Thus, below, they are
%% written if found.
insert_(Tab, AID, DID, Options) ->
    insert(Tab,DID, 'did',  exodm_db:decode_id(DID)),
    insert_keys(Tab, DID, Options),
    insert_groups(AID, Tab, DID, proplists:get_value(groups, Options, [])),
    insert_device_type(
      Tab, AID, DID, proplists:get_value('device-type', Options)),
    insert_protocol(Tab, DID, proplists:get_value(protocol, Options)),
    _ = [insert_attr(Tab, DID, K,to_binary(V)) ||
	    {K,V} <- Options,
	    not lists:member(K, ['device-id','gid',
				 'server-key', 'device-key',
				 'protocol', 'groups'])],
    ok.


required(Keys, Options) ->
    lists:foreach(fun(K) -> required_(K, Options) end, Keys).

required_(K, Options) ->
    case lists:keymember(K, 1, Options) of
	true -> true;
	false -> error({required, K})
    end.

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
%% lists:foreac Nh(
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
    ok;
insert_protocol(Tab, DID, Protocol) ->
    case exodm_rpc_protocol:module(Protocol) of
	undefined ->
	    error(unknown_protocol, [Protocol]);
	_ ->
	    insert(Tab, DID, protocol, to_binary(Protocol))
    end.

insert_device_type(_Tab, _AID, _DID, undefined) ->
    ok;
insert_device_type(Tab, AID, DID, DevType) ->
    case exodm_db_device_type:exist(AID, DevType) of
	undefined ->
	    error(unknown_device_type, [DevType]);
	_ ->
	    insert(Tab, DID, device_type, to_binary(DevType))
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
    exodm_db:in_transaction(
      fun(_) ->
	      lookup_(table(AID), exodm_db:encode_id(DID))
      end).

lookup_(Tab,Key) ->
    case kvdb_conf:read(Tab, kvdb_conf:join_key(Key, <<"did">>)) of
	{ok, {_, _, _}} ->
	    [{'dev-id', exodm_db:decode_id(Key)} |
	     read(Tab,Key,protocol)  ++
		 lookup_attrs(Tab, Key)];
	{error, _} ->
	    []
    end.

lookup_attrs(Tab, Key) ->
    Res = kvdb_conf:fold_children(
	    Tab, fun(K, Acc) ->
			 case kvdb_conf:read(Tab, K) of
			     {ok, {_, _, V}} ->
				 [{lists:last(kvdb_conf:split_key(K)), V}|Acc];
			     _ ->
				 Acc
			 end
		 end, [], kvdb_conf:join_key(Key, <<"a">>)),
    lists:reverse(Res).

%% ++
%% 	read(Tab,Key, msisdn) ++
%% 	read(Tab,Key, imsi) ++
%% 	read(Tab,Key, imei) ++
%% 	read(Tab,Key, 'device-key') ++
%% 	read(Tab,Key, 'server-key') ++
%% 	read(Tab,Key, yang) ++
%% 	read_uint32(Tab,Key, activity) ++
%% 	read_uint32(Tab,Key, status) ++
%% 	read_uint32(Tab,Key, longitude) ++
%% 	read_uint32(Tab,Key, latitude) ++
%% 	read_uint32(Tab,Key, timestamp).

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
    case kvdb_conf:read_tree(Tab, kvdb_conf:join_key(DID, <<"groups">>)) of
	#conf_tree{tree = T} ->
	    [GID || {GID,_, _} <- T];
	_ ->
	    []
    end.
    %% lists:reverse(
    %%   exodm_db:fold_list2(
    %% 	Tab,
    %% 	fun(I,Key,Acc) ->
    %% 		case read_uint32(Tab, Key, 'gid') of
    %% 		    [{_,GID}] -> [{group,{I,GID}} | Acc];
    %% 		    [] -> Acc
    %% 		end
    %% 	end, [], DID, groups)).


lookup_group_notifications(AID0, DID0) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    lists:flatmap(
      fun(GID) ->
	      case exodm_db_group:lookup(AID, GID, url) of
		  [{_, URL}] ->
		      [URL];
		  [] ->
		      []
	      end
      end, lookup_groups(AID, DID)).
    %% lists:foldl(
    %%   fun({group,{_I,GID0}},Acc) ->
    %% 	      GID = exodm_db:group_id_key(GID0),
    %% 	      case exodm_db_group:lookup(AID, GID, url) of
    %% 		  [{_, URL}] ->
    %% 		      [URL | Acc];
    %% 		  [] ->
    %% 		      Acc
    %% 	      end
    %%   end,[],lookup_groups(AID, DID)).

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
%%     insert_groups(AID, Tab, 1, DID, Groups).

%% insert_groups(AID, Tab, I0, DID, Groups0) ->
    lists:foreach(fun(G) ->
			  insert_group_(AID, Tab, DID, G)
		  end, Groups).
    %% {Groups,_} = lists:mapfoldl(fun(G,I) ->
    %% 					{{I,G}, I+1}
    %% 				end, I0, Groups0),
    %% lists:foreach(fun({I,G}) ->
    %% 			  insert_group(AID, Tab, DID, I, G)
    %% 		  end, Groups).

insert_group_(AID, Tab, DID, GID) ->
    K = exodm_db:join_key([DID, <<"groups">>, exodm_db:group_id_key(GID)]),
    exodm_db_group:add_device(AID, GID, DID),
    kvdb_conf:write(Tab, {K, [], <<>>}).

%% insert_group(AID, Tab, DID, I, GID) when is_integer(I) ->
%%     K = exodm_db:join_key(DID, exodm_db:list_key(groups, I)),
%%     exodm_db_group:add_device(AID, GID, DID),
%%     insert(Tab, K, 'gid',  gid_value(GID)).

remove_group(AID, Tab, DID, I, GID) ->
    K = exodm_db:join_key(DID, exodm_db:list_key(groups, I)),
    exodm_db_group:remove_device(AID, GID, DID),
    kvdb_conf:delete_tree(Tab, K).

add_groups(AID, DID0, Groups) ->
    DID = exodm_db:encode_id(DID0),
    Tab = table(AID),
    exodm_db:in_transaction(
      fun(_) ->
	      case exist(AID, DID) of
		  true ->
		      add_groups_(Tab, AID, DID, Groups);
		  false ->
		      error(unknown_device)
	      end
      end).

add_groups_(Tab, AID, DID, Groups) ->
    lists:foreach(
      fun(GID0) ->
	      GID = exodm_db:group_id_key(GID0),
	      Key = kvdb_conf:join_key([DID, <<"groups">>, GID]),
	      kvdb_conf:write(Tab, {Key, [], <<>>}),
	      exodm_db_group:add_device(AID, GID, DID)
      end, Groups).
      %% 	      {Existing, Last} =
      %% 		  kvdb_conf:fold_list(
      %% 		    Tab, fun(I, Kl, {Acc,_}) ->
      %% 				 {ok,{_,_,<<GID:32>>}} =
      %% 				     kvdb_conf:read(
      %% 				       Tab, kvdb_conf:join_key(
      %% 					      Kl, <<"gid">>)),
      %% 				 case lists:member(GID, Groups) of
      %% 				     true ->
      %% 					 {[GID|Acc],I};
      %% 				     false ->
      %% 					 {Acc,I}
      %% 				 end
      %% 			 end, {[],0}, kvdb_conf:join_key(DID,<<"groups">>)),
      %% 	      ToAdd = Groups -- Existing,
      %% 	      insert_groups(AID, Tab, Last+1, DID, ToAdd)
      %% end).

remove_groups(AID, DID0, Groups) ->
    DID = exodm_db:encode_id(DID0),
    Tab = table(AID),
    exodm_db:in_transaction(
      fun(_) ->
	      case exist(AID, DID) of
		  true ->
		      remove_groups_(Tab, AID, DID, Groups);
		  false ->
		      error(unknown_device)
	      end
      end).

remove_groups_(Tab, AID, DID, Groups) ->
    lists:foreach(
      fun(GID0) ->
	      GID = exodm_db:group_id_key(GID0),
	      Key = kvdb_conf:join_key([DID, <<"groups">>, GID]),
	      kvdb_conf:delete(Tab, Key),
	      exodm_db_group:remove_device(AID, GID, DID)
      end, Groups).
    %% 	      Found =
    %% 		  kvdb_conf:fold_list(
    %% 		    Tab, fun(I, Kl, Acc) ->
    %% 				 {ok,{_,_,<<GID:32>>}} =
    %% 				     kvdb_conf:read(
    %% 				       Tab, kvdb_conf:join_key(
    %% 					      Kl, <<"gid">>)),
    %% 				 case lists:member(GID, Groups) of
    %% 				     true ->
    %% 					 [{I,GID}|Acc];
    %% 				     false ->
    %% 					 Acc
    %% 				 end
    %% 			 end, [], kvdb_conf:join_key(DID,<<"groups">>)),
    %% 	      lists:foreach(
    %% 		fun({I, GID}) ->
    %% 			remove_group(AID, Tab, DID, I, GID)
    %% 		end, Found)
    %%   end),
    %% ok.

gid_value(GID) ->
    <<(exodm_db:group_id_num(GID)):32>>.

read(Tab, Key, Item) ->
    read_(Tab, Item, exodm_db:join_key([Key, to_binary(Item)])).

%% read(Tab, Key, Sub, Item) ->
%%     read_(Tab, Item, exodm_db:join_key([Key, Sub, to_binary(Item)])).

read_(Tab, Item, Key) ->
    case exodm_db:read(Tab, Key) of
	{ok,{_,_,Value}} -> [{Item,Value}];
	{error,not_found} -> []
    end.

read_uint32(Tab, Key,Item) ->
    case read(Tab, Key,Item) of
	[{Item,<<Value:32>>}] -> [{Item,Value}];
	[] -> []
    end.


code_change(FromVsn, ToVsn) ->
    Tabs = kvdb:list_tables(kvdb_conf),
    %% exodm_db:in_transaction(
    %%   fun(_) ->
	      [{T,ok} = {T, transform_tab(T, FromVsn, ToVsn)}
	       || T <- Tabs, is_dev_table(T)].
      %% end).

is_dev_table(T) ->
    Sz = byte_size(T),
    PSz = Sz - 4,
    case T of
	<<_:PSz/binary, "_dev">> ->
	    true;
	_ ->
	    false
    end.

transform_tab(T, FromVsn, ToVsn) ->
    case kvdb_conf:first(T) of
	{ok, {Key, _, _}} ->
	    transform_tab({ok, Key}, T, FromVsn, ToVsn);
	done ->
	    ok
    end.

transform_tab({ok, Key}, T, From, To) ->
    [Top|_] = kvdb_conf:split_key(Key),
    #conf_tree{} = CT = kvdb_conf:read_tree(T, Top),
    kvdb_conf:delete_tree(T, Top),
    kvdb_conf:write_tree(T, Top, transform_tree(CT, From, To)),
    transform_tab(kvdb_conf:next_at_level(T, Top), T, From, To);
transform_tab(done, _, _, _) ->
    ok.



transform_tree(#conf_tree{tree = T} = CT, _, _) ->
    T1 = lists:map(
	   fun({<<"__did">>, [], DID}) ->
		   {<<"did">>, [], DID};
	      ({K, _, _} = X) ->
		   case lists:member(K, [<<"config_set">>, <<"device-key">>,
					 <<"server-key">>, <<"did">>,
					 <<"protocol">>]) of
		       true ->
			   X;
		       false ->
			   {a,X}
		   end;
	      ({<<"config_set">>, L} = X) when is_list(L) ->
		   X;
	      ({<<"groups">>, L} = X) when is_list(L) ->
		   X
	   end, T),
    Attrs = [C || {a, C} <- T1],
    CT#conf_tree{tree = lists:sort([{<<"a">>,Attrs}|
				    [X || X <- T1,
					  element(1,X) =/= a]])}.
