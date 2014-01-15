%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense device manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_device).

-export([init/1]).
-export([new/3, update/3, lookup/2, lookup_attr/3, lookup_attrs/3,
	 list_next/3,
	 write_attrs/3,
	 delete/2, delete_devices/2,
	 add_config_set/3, remove_config_set/3, list_config_sets/2,
	 yang_modules/2,
	 protocol/2,
	 push_protocol/2,
	 exist/2]).
-export([key/2, tab_and_key/1]).
-export([enc_ext_key/2, dec_ext_key/1]).
-export([lookup_position/2, lookup_keys/2]).
-export([add_groups/3, lookup_groups/2, remove_groups/3]).
-export([do_add_group/3, do_remove_group/3]).
-export([lookup_group_notifications/2]).
-export([table/1]).
-export([client_auth_config/2]).

-export([add_request_url/4, delete_request_url/3]).

-export([transform/0]).
-export([code_change/2]).

-include_lib("lager/include/log.hrl").
-include_lib("kvdb/include/kvdb_conf.hrl").
-include("exodm_db.hrl").

-import(exodm_db, [write/2, binary_opt/2, uint32_opt/2, to_binary/1]).
-import(lists, [reverse/1]).

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
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(ID0),
    exodm_db:all_required(['device-type'], Options),
    Tab = table(AID),
    insert_(Tab, AID, DID, Options).

%% Called at both new() and update(). Device-type and protocol are not required
%% at update, but presence verified in new_/3 above. Thus, below, they are
%% written if found.
insert_(Tab, AID, DID, Options) ->
    insert(Tab,DID, 'device-id',  exodm_db:decode_id(DID)),
    insert_keys(Tab, DID, Options),
    insert_groups(AID, Tab, DID, proplists:get_value(groups, Options, [])),
    insert_device_type(
      Tab, AID, DID, proplists:get_value('device-type', Options)),
    _ = [insert_attr(Tab, DID, K, V) ||
	    {K,V} <- Options,
	    not lists:member(K, ['device-id','group-id', 'device-type',
				 'server-key', 'device-key',
				 'protocol', 'groups'])],
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
    remove_devtype(Tab, AID, DID),
    kvdb_conf:delete_tree(Tab, DID),
    ConfigSets = list_config_sets(AID, DID),
    exodm_db_config:device_is_deleted(AID, DID, ConfigSets),
    Groups = lookup_groups(AID, DID),
    exodm_db_group:device_is_deleted(AID, DID, Groups).

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

%% For internal use: write without validation
write_attrs(AID0, DID0, Attrs) ->
    DID = exodm_db:encode_id(DID0),
    AID = exodm_db:account_id_key(AID0),
    Tab = table(AID),
    case exist(AID, DID) of
	true ->
	    lists:foreach(
	      fun({K, V}) ->
		      insert_attr(Tab, DID, K, V) 
	      end, Attrs);
	false ->
	    error(device_not_found)
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

%% This function stores a device-specific request URL that shadows a given
%% config spec
add_request_url(AID0, DID0, CS0, URL) ->
    exodm_db:in_transaction(
      fun(_) ->
	      AID = exodm_db:account_id_key(AID0),
	      DID = exodm_db:encode_id(DID0),
	      CS = exodm_db:encode_id(CS0),
	      Tab = table(AID),
	      case exist(AID, DID) of
		  true ->
		      CfgKey = exodm_db:join_key([DID, ?DEV_DB_CONFIG_SET, CS]),
		      case kvdb_conf:read(Tab, CfgKey) of
			  {ok, _} ->
			      Key = kvdb_conf:join_key(
				      [DID, ?DEV_DB_REQ_URLS, CS]),
			      kvdb_conf:write(Tab, {Key, [], URL});
			  {error,_} ->
			      error(unknown_config_set)
		      end;
		  false ->
		      error(unknown_device)
	      end
      end).

delete_request_url(AID0, DID0, CS0) ->
    exodm_db:in_transaction(
      fun(_) ->
	      AID = exodm_db:account_id_key(AID0),
	      DID = exodm_db:encode_id(DID0),
	      CS = exodm_db:encode_id(CS0),
	      Tab = table(AID),
	      case exist(AID, DID) of
		  true ->
		      CfgKey = exodm_db:join_key([DID, ?DEV_DB_CONFIG_SET, CS]),
		      case kvdb_conf:read(Tab, CfgKey) of
			  {ok, _} ->
			      Key = kvdb_conf:join_key(
				      [DID, ?DEV_DB_REQ_URLS, CS]),
			      kvdb_conf:delete(Tab, Key);
			  {error,_} ->
			      error(unknown_config_set)
		      end;
		  false ->
		      error(unknown_device)
	      end
      end).


insert_device_type(_Tab, _AID, _DID, undefined) ->
    ok;
insert_device_type(Tab, AID, DID, DevType) ->
    case exodm_db_device_type:exist(AID, DevType) of
	false ->
	    error(unknown_device_type, [DevType]);
	_ ->
	    exodm_db_device_type:add_device(AID, DevType, DID),
	    insert(Tab, DID, 'device-type', to_binary(DevType))
    end.

delete(AID0, DID0) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    %% Tab = table(AID),
    exodm_db:in_transaction(
      fun(_) -> delete_device(AID, DID) end).

remove_devtype(Tab, AID, DID) ->
    case kvdb_conf:read(Tab, kvdb_conf:join_key(DID, ?DEV_DB_DEVICE_TYPE)) of
	{ok, {_, _, T}} ->
	    exodm_db_device_type:remove_device(AID, T, DID);
	{error, not_found} ->
	    %% Can this happen?
	    ok
    end.

add_config_set(AID, DID, CfgName) ->
    exodm_db:in_transaction(fun(_) -> add_config_set_(AID, DID, CfgName) end).

add_config_set_(AID0, DID0, CfgName) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    Tab = table(AID),
    case exist(AID, DID) of
	true ->
	    insert(Tab, exodm_db:join_key(DID, ?DEV_DB_CONFIG_SET),
		   CfgName, <<>>);
	false ->
	    error({unknown_device, [AID, DID]})
    end.

remove_config_set(AID, DID, CfgName) ->
    exodm_db:in_transaction(fun(_) ->
				    remove_config_set_(AID, DID, CfgName)
			    end).

remove_config_set_(AID0, DID0, CfgName) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    Tab = table(AID),
    case exist(AID, DID) of
	true ->
	    kvdb_conf:delete(Tab, kvdb_conf:join_key(
				    [DID, ?DEV_DB_CONFIG_SET, CfgName])),
	    kvdb_conf:delete(Tab, kvdb_conf:join_key(
				    [DID, ?DEV_DB_REQ_URLS, CfgName]));
	false->
	    ok
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
		 end, [], exodm_db:join_key(DID, ?DEV_DB_CONFIG_SET)),
    lists:reverse(Set).


yang_modules(AID, DID) when is_binary(AID), is_binary(DID) ->
    ?debug("aid ~p, did ~p", [AID, DID]),
    Tab = table(AID),
    exodm_db:in_transaction(
      fun(_) ->
	      CDs = list_config_sets_(Tab, DID),
              ?debug("Config sets = ~p~n", [CDs]),
	      lists:map(
		fun(Name) ->
			{ok, Y} = exodm_db_config:get_yang_spec(AID, Name),
			URL = case exodm_db_config:get_url(AID, Name) of
				  {ok, U} -> U;
				  {error, not_found} -> <<>>
			      end,
			{Name, Y, URL}
		end, CDs)
      end);
yang_modules(AID0, DID0) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    yang_modules(AID, DID).


protocol(AID, DID) when is_binary(AID), is_binary(DID) ->
    Tab = table(AID),
    case read_value(Tab, DID, ?DEV_DB_DEVICE_TYPE) of
	false ->
	    %% BW compatibility; remove soon
	    case read_value(Tab, DID, ?DEV_DB_PROTOCOL) of
		false ->
		    error(no_protocol_defined, [AID, DID]);
		P ->
		    P
	    end;
	T ->
	    case kvdb_conf:read(
		   exodm_db_device_type:table(AID),
		   kvdb_conf:join_key(T, <<"protocol">>)) of
		{ok, {_, _, P}} -> P;
		{error, not_found} ->
		    error(no_protocol_defined, [AID, DID])
	    end
    end;
protocol(AID0, DID0) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    protocol(AID, DID).

push_protocol(AID, DID) when is_binary(AID), is_binary(DID) ->
    Tab = table(AID),
    case read_value(Tab, DID, ?DEV_DB_PUSH_PROTOCOL) of
	false ->
	    case read_value(Tab, DID, ?DEV_DB_DEVICE_TYPE) of
		false ->
		    none;
		T ->
		    case kvdb_conf:read(
			   exodm_db_device_type:table(AID),
			   kvdb_conf:join_key(T, ?DEV_DB_PUSH_PROTOCOL)) of
			{ok, {_, _, P}} ->
			    P;
			{error, not_found} ->
			    none
		    end
	    end;
	P ->
	    P
    end.

list_next(AID, N, Prev) when is_binary(AID), is_binary(Prev) ->
    exodm_db:list_next(table(AID), N, Prev,
		       fun(Key) ->
			       [DID|_] = kvdb_conf:split_key(Key),
			       lookup(AID, DID)
		       end).

lookup(AID, DID) ->
    exodm_db:in_transaction(
      fun(_) ->
	      lookup_(table(AID), exodm_db:encode_id(DID))
      end).

lookup_(Tab,Key) ->
    case kvdb_conf:read(Tab, kvdb_conf:join_key(Key, ?DEV_DB_DEVICE_ID)) of
	{ok, {_, _, _}} ->
	    [{'device-id', exodm_db:decode_id(Key)} |
	     lookup_attr_(Tab,Key,'device-type') ++
		 lookup_attrs(Tab, Key)];
	{error, _} ->
	    []
    end.

lookup_attrs(Tab, Key) ->
    Res = kvdb_conf:fold_children(
	    Tab, fun(K, Acc) ->
			 case kvdb_conf:read(Tab, K) of
			     {ok, {_, _, V}} ->
				 [convert_attr(
				    {lists:last(kvdb_conf:split_key(K)), V})
				  | Acc];
			     _ ->
				 Acc
			 end
		 end, [], Key),
    remove_keys(lists:reverse(Res)).

remove_keys(L) ->
    [Obj || {K,_} = Obj <- L,
	    K =/= ?DEV_DB_DEVICE_KEY,
	    K =/= ?DEV_DB_SERVER_KEY].

convert_attr({K,V}) ->
    {K, decode_value(K, V)}.

lookup_attrs(AID, DID, Attrs) when is_list(Attrs) ->
    lookup_attrs(AID, DID, Attrs, []).

lookup_attrs(_AID, _DID, [], Acc) ->
    Acc;
lookup_attrs(AID, DID, [Attr | Rest] = Attrs, Acc) ->
    lager:debug("lookup_attrs: ~p ~p ~p ~p", [AID, DID, Attrs, Acc]),
    %% lookup_attr/3 returns a tuple-list.
    lookup_attrs(AID, DID, Rest, 
		 lookup_attr(AID, DID, Attr) ++ Acc).

lookup_attr(AID, DID, Attr) when is_atom(Attr); is_binary(Attr) ->
    lager:debug("lookup_attrs: ~p ~p ~p", [AID, DID, Attr]),
    lookup_attr_(table(AID), DID, Attr).

lookup_attr_(Tab, DID, Attr) when is_atom(Attr); is_binary(Attr) ->
    case read_value(Tab, exodm_db:encode_id(DID), to_binary(Attr)) of
	false -> [];
	Value -> [{Attr,Value}]
    end.


lookup_groups(AID, DID0) ->
    Tab = table(AID),
    DID = exodm_db:encode_id(DID0),
    case kvdb_conf:read_tree(Tab, kvdb_conf:join_key(DID,?DEV_DB_GROUPS)) of
	#conf_tree{tree = T} ->
	    [GID || {GID,_, _} <- T];
	_ ->
	    []
    end.


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
      end, lookup_groups(AID, DID)) ++
	lists:flatmap(
	  fun(CS) ->
		  CSTab = exodm_db_config:table(AID),
		  get_notification_url(AID, DID, CSTab, CS)
	  end, list_config_sets(AID, DID)).

get_notification_url(AID, DID, CSTab, CS) ->
    case kvdb_conf:read(table(AID), kvdb_conf:join_key(
				      [DID, ?DEV_DB_REQ_URLS, CS])) of
	{ok, {_, _, URL}} -> [URL];
	{error, _} ->
	    case kvdb_conf:read(CSTab, kvdb_conf:join_key(CS,<<"url">>)) of
		{ok, {_, _, URL}} ->
		    [URL];
		_ ->
		    []
	    end
    end.


%% find last known position or {0,0,0} if not found
lookup_position(AID, DID0) ->
    Tab = table(AID),
    DID = exodm_db:encode_id(DID0),
    {
      case read_value(Tab,DID,?DEV_DB_LATITUDE) of
	  false -> 0.0;
	  Lat -> exodm_db:bin_to_float(Lat)
      end,
      case read_value(Tab,DID,?DEV_DB_LONGITUDE) of
	  false -> 0.0;
	  Lon -> exodm_db:bin_to_float(Lon)
      end,
      case read_value(Tab,DID,?DEV_DB_TIMESTAMP) of
	  false -> 0;
	  <<Ts:32>> -> Ts
      end
    }.

dec_ext_key(<<$a, Ia:8/binary, "=", Ix/binary>>) ->
    ?debug("Ia = ~p, Ix = ~p~n", [Ia,Ix]),
    {<<"a", Ia/binary>>, <<"=", Ix/binary>>};
dec_ext_key(<<Sep, ID/binary>>) ->
    ?debug("Sep = ~p, ID = ~p~n", [Sep, ID]),
    case split(Sep, ID) of
	[AcctName, DID] ->
	    ?debug("Name = ~p, DID = ~p~n", [AcctName, DID]),
	    case exodm_db_account:lookup_by_name(AcctName) of
	        false->
		    error;
		AID ->
		    {AID, exodm_db:encode_id(DID)}
	    end;
	_ ->
	    error
    end;
dec_ext_key(Key) ->
    ?debug("Key = ~p~n", [Key]),
    try
	AID = exodm_db_session:get_aid(),
	{exodm_db:account_id_key(AID), exodm_db:encode_id(Key)}
    catch
	error:_ ->
	    error
    end.

split(Sep, Bin) ->
    split(Sep, Bin, <<>>).

split(C, <<C, Rest/binary>>, Acc) ->
    [Acc, Rest];
split(C, <<H, T/binary>>, Acc) ->
    split(C, T, <<Acc/binary, H>>);
split(_, <<>>, _) ->
    [].


enc_ext_key(<<$a,_/binary>> = AID, <<$=, _/binary>> = DID) ->
    <<AID/binary, DID/binary>>;
enc_ext_key(<<$a,_/binary>> = AID, DID) when is_integer(DID)->
    BinDID = exodm_db:encode_id(DID),
    <<AID/binary, BinDID/binary>>;
enc_ext_key(AID, DID) ->
    enc_ext_key(exodm_db:account_id_key(AID), exodm_db:encode_id(DID)).


%% @doc Convenience function to extract a client auth config for the
%% device BERT RPC client.
client_auth_config(AID0, DID0) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    Tab = table(AID),
    case read_value(Tab, DID, ?DEV_DB_DEVICE_KEY) of
	false -> {error, not_found};
	Ck ->
	    case read_value(Tab, DID, ?DEV_DB_SERVER_KEY) of
		false -> {error, not_found};
		Cs ->
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
      case read_value(Tab, DID, ?DEV_DB_DEVICE_KEY) of
	  false -> <<0,0,0,0,0,0,0,0>>;
	  Ck -> Ck
      end,
      case read_value(Tab, DID, ?DEV_DB_SERVER_KEY) of
	  false ->  <<0,0,0,0,0,0,0,0>>;
	  Sk -> Sk
      end}.

exist(AID, DID) ->
    lager:debug("(AID=~p, DID=~p)~n", [AID,DID]),
    case read_value(table(AID), exodm_db:encode_id(DID), ?DEV_DB_DEVICE_ID) of
	false -> false;
	_ -> true
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
    ItemB = to_binary(Item),
    Key1 = exodm_db:join_key(Key, to_binary(Item)),
    exodm_db:write(Tab, Key1, encode_value(ItemB, Value)).

encode_value(?DEV_DB_LATITUDE, L) when is_number(L) ->
    exodm_db:float_to_bin(L);
encode_value(?DEV_DB_LONGITUDE, L) when is_number(L) ->
    exodm_db:float_to_bin(L);
encode_value(?DEV_DB_TIMESTAMP, L) when is_number(L) ->
    exodm_db:uint_to_bin(L);
encode_value(?DEV_DB_SESSION_TIMEOUT, T) ->
    list_to_binary(integer_to_list(T));
encode_value(?DEV_DB_PASSWORD, P0) ->
    P = to_binary(P0),
    {ok, Salt} = gen_salt(),
    {ok, Hash} = bcrypt:hashpw(P, Salt),
    to_binary(Hash);
encode_value(_, V) ->
    to_binary(V).

gen_salt() ->
    case application:get_env(bcrypt, default_log_rounds) of
	{ok, R} -> bcrypt:gen_salt(R);
	_ -> bcrypt:gen_salt()
    end.

decode_value(?DEV_DB_LATITUDE, Bin) ->
    exodm_db:bin_to_float(Bin);
decode_value(?DEV_DB_LONGITUDE, Bin) ->
    exodm_db:bin_to_float(Bin);
decode_value(?DEV_DB_TIMESTAMP, Bin) ->
    exodm_db:bin_to_uint(Bin);
decode_value(?DEV_DB_SESSION_TIMEOUT, Bin) ->
    list_to_integer(binary_to_list(Bin));
decode_value(_, Bin) ->
    Bin.



insert_groups(AID, Tab, DID, Groups) ->
    lists:foreach(fun(G) ->
			  insert_group_(AID, Tab, DID, G)
		  end, Groups).


insert_group_(AID, Tab, DID, GID) ->
    K = exodm_db:join_key([DID, ?DEV_DB_GROUPS, exodm_db:encode_id(GID)]),
    exodm_db_group:add_device(AID, GID, DID),
    kvdb_conf:write(Tab, {K, [], <<>>}).


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
	      GID = exodm_db:encode_id(GID0),
	      Key = kvdb_conf:join_key([DID, ?DEV_DB_GROUPS, GID]),
	      kvdb_conf:write(Tab, {Key, [], <<>>}),
	      exodm_db_group:add_device(AID, GID, DID)
      end, Groups).

do_add_group(AID0, DID0, GID0) ->
    ?debug("~p:do_add_group(AID=~p, DID=~p, GID=~p)~n", [?MODULE,AID0,DID0,GID0]),
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    GID = exodm_db:encode_id(GID0),
    Tab = table(AID),
    exodm_db:in_transaction(
      fun(_) ->
	      case exist(AID, DID) of
		  true ->
		      kvdb_conf:write(Tab, {kvdb_conf:join_key(
					      [DID,?DEV_DB_GROUPS,GID]),
					    [], <<>>});
		  false ->
		      error(unknown_device)
	      end
      end).


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
	      GID = exodm_db:encode_id(GID0),
	      Key = kvdb_conf:join_key([DID,?DEV_DB_GROUPS,GID]),
	      kvdb_conf:delete(Tab, Key),
	      exodm_db_group:remove_device(AID, GID, DID)
      end, Groups).

do_remove_group(AID0, DID0, GID0) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    GID = exodm_db:encode_id(GID0),
    Tab = table(AID),
    exodm_db:in_transaction(
      fun(_) ->
	      case exist(AID, DID) of
		  true ->
		      kvdb_conf:delete(Tab, kvdb_conf:join_key(
					      [DID,?DEV_DB_GROUPS,GID]));
		  false ->
		      error
	      end
      end).

read_value(Tab, Key, Item) when is_binary(Item) ->
    case exodm_db:read(Tab, exodm_db:join_key(attr_key(Key, Item))) of
	{ok,{_,_,Value}} -> Value;
	{error,not_found} -> false
    end.
    
attr_key(Key, <<"did">>        ) -> [Key, ?DEV_DB_DEVICE_ID];
attr_key(Key, <<"device-id">>     ) -> [Key, ?DEV_DB_DEVICE_ID];
attr_key(Key, A) -> [Key, A].


transform() ->
    code_change(exodm_db:get_db_version(), exodm_db:get_system_version()).

code_change(FromVsn, ToVsn) ->
    %% io:fwrite("code_change(~p, ~p)~n", [FromVsn, ToVsn]),
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
    case transform_tree(CT, From, To) of
	skip -> ok;
	#conf_tree{} = NewCT ->
	    kvdb_conf:delete_tree(T, Top),
	    kvdb_conf:write_tree(T, Top, NewCT)
    end,
    transform_tab(kvdb_conf:next_at_level(T, Top), T, From, To);
transform_tab(done, _, _, _) ->
    ok.

transform_tree(#conf_tree{tree = T} = CT, _, _) ->
    %% io:fwrite("ConfTree = ~p~n", [CT]),
    NewCT =
	case lists:keyfind(<<"a">>, 1, T) of
	    {<<"a">>, L} ->
		rename_did(
		  CT#conf_tree{tree = lists:keysort(
					1, lists:keydelete(<<"a">>, 1, T) ++ L)});
	    false ->
		rename_did(CT)
	end,
    %% io:fwrite("NewCT = ~p~n", [NewCT]),
    NewCT.

rename_did(#conf_tree{tree = T} = CT) ->
    NewT = lists:map(
	     fun({<<"did">>, As, V}) ->
		     {<<"device-id">>, As, V};
		(X) -> X
	     end, T),
    CT#conf_tree{tree = NewT}.
