-module(exodm_db_device_type).

-export([init/1]).
-export([
	 new/3,
	 lookup/2,
	 update/3,
	 delete/2,
	 exist/2,
	 add_device/3,
	 remove_device/3,
	 list_next/3,
	 list_devices/4,
	 table/1
	]).

-include_lib("kvdb/include/kvdb_conf.hrl").
-include_lib("lager/include/log.hrl").
-include("exodm_db.hrl").

init(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:in_transaction(
      fun(_) ->
	      kvdb_conf:add_table(table(AID), [])
      end).

table(AID) ->
    table_(exodm_db:account_id_key(AID)).

table_(AID) ->
    <<AID/binary, "_devtype">>.

new(AID0, Name0, Options) ->
    ?debug("~p:new(~p, ~p, ~p)~n", [?MODULE, AID0, Name0, Options]),
    AID = exodm_db:account_id_key(AID0),
    Name = exodm_db:encode_id(Name0),
    Tab = table_(AID),
    true = exodm_db:all_required([protocol], Options),
    {_, Protocol0} = lists:keyfind(protocol, 1, Options),
    Protocol = to_binary(Protocol0),
    valid_protocol(Protocol),
    PushProtocol =
	case lists:keyfind('push-protocol', 1, Options) of
	    {_, PushProto0} ->
		PushProto = to_binary(PushProto0),
		valid_protocol(PushProto),
		[{?DEV_DB_PUSH_PROTOCOL, [], PushProto}];
	    false ->
		[]
	end,
    Tree = #conf_tree{root = Name,
		      tree = [{<<"name">>, [], exodm_db:decode_id(Name)},
			      {?DEV_DB_PROTOCOL, [], Protocol}
			      | PushProtocol]},
    exodm_db:in_transaction(
      fun(_) ->
	      case exist_(AID, Name) of
		  false ->
		      kvdb_conf:write_tree(Tab, Name, Tree);
		  true ->
		      error({exists, Name})
	      end
      end),
    ok.

lookup(AID0, Name0) ->
    {AID, Name} = encode(AID0, Name0),
    exodm_db:in_transaction(
      fun(_) ->
	      Tab = table_(AID),
	      case read(Tab, Name, <<"name">>) of
		  [] -> [];
		  [N] ->
		      [N | read(Tab, Name, <<"protocol">>)]
	      end
      end).

read(Tab, Name, Attr) ->
    case kvdb_conf:read(Tab, kvdb_conf:join_key(Name, Attr)) of
	{ok, {_, _, V}} ->
	    [{Attr, V}];
	{error, not_found} ->
	    []
    end.

update(AID0, Name0, Options) ->
    ?debug("~p:new(~p, ~p, ~p)~n", [?MODULE, AID0, Name0, Options]),
    AID = exodm_db:account_id_key(AID0),
    Name = exodm_db:encode_id(Name0),
    Tab = table_(AID),
    Values = lists:map(
	       fun({'protocol', P0}) ->
		       P = to_binary(P0),
		       valid_protocol(P),
		       {?DEV_DB_PROTOCOL, [], P}
	       end, Options),
    Tree = #conf_tree{root = Name,
		      tree = Values},
    exodm_db:in_transaction(
      fun(_) ->
	      case exist_(AID, Name) of
		  true ->
		      kvdb_conf:write_tree(Tab, Name, Tree);
		  false ->
		      error('object-not-found')
	      end
      end),
    ok.

valid_protocol(P) ->
    case exodm_rpc_protocol:module(P) of
	undefined ->
	    error('validation-error');
	_ ->
	    ok
    end.

delete(AID0, Name0) ->
    ?debug("~p, ~p", [AID0, Name0]),
    {AID, Name} = encode(AID0, Name0),
    Tab = table_(AID),
    exodm_db:in_transaction(
      fun(_) ->
	      case exist_(AID, Name) of
		  true ->
		      delete_(Tab, Name);
		  false ->
		      ok
	      end
      end).

delete_(Tab, Name) ->
    Name0 = kvdb_conf:unescape_key_part(Name),
    case kvdb_conf:next(
	   Tab, kvdb_conf:join_key(Name, <<"devices">>)) of
	done -> kvdb_conf:delete_tree(Tab, Name);
	{ok, {K, _, _}} ->
	    case kvdb_conf:split_key(K) of
		[Name0, <<"devices">>| Devices] ->
                    ?debug("config set ~p not empty, devices ~p", 
                           [Name, Devices]),
		    error('object-not-empty');
		_ ->
		    kvdb_conf:delete_tree(Tab, Name)
	    end
    end.

exist(AID0, Name0) ->
    {AID,Name} = encode(AID0, Name0),
    exist_(AID, Name).

exist_(AID, Name) ->
    case kvdb_conf:read(table_(AID), kvdb_conf:join_key(Name, <<"name">>)) of
	{ok, _} ->
	    true;
	_ ->
	    false
    end.

add_device(AID0, Name0, DID0) ->
    {AID, Name} = encode(AID0, Name0),
    DID = exodm_db:decode_id(exodm_db:encode_id(DID0)),
    Tab = table_(AID),
    exodm_db:in_transaction(
      fun(_) ->
	      case exist_(AID, Name) of
		  true ->
		      kvdb_conf:write(
			Tab, {kvdb_conf:join_key([Name, <<"devices">>, DID]),
			      [], <<>>});
		  false ->
		      error('object-not-found')
	      end
      end).

remove_device(AID0, Name0, DID0) ->
    {AID, Name} = encode(AID0, Name0),
    DID = exodm_db:decode_id(exodm_db:encode_id(DID0)),
    Tab = table_(AID),
    exodm_db:in_transaction(
      fun(_) ->
	      case exist_(AID, Name) of
		  true ->
		      kvdb_conf:delete(
			Tab, kvdb_conf:join_key([Name, <<"devices">>, DID]));
		  false ->
		      error('object-not-found')
	      end
      end).


list_next(AID, N, Prev) ->
    exodm_db:in_transaction(
	fun(_) ->
	    exodm_db:list_next(exodm_db_device_type:table(AID), N, Prev,
		fun(Key) ->
		    [Name|_] =
		    kvdb_conf:split_key(Key),
		    exodm_db_device_type:lookup(
			AID, Name)
		end)
	end).

list_devices(AID, Name, N, Prev) ->
    exodm_db:in_transaction(
	fun(_) ->
	    FullNext = kvdb_conf:join_key(
		[Name, <<"devices">>, Prev]),
	    exodm_db:list_next(exodm_db_device_type:table(AID),
		N, FullNext,
		fun(Key) ->
		    lists:last(
			kvdb_conf:split_key(Key))
		end)
	end).

encode(AID, Name) ->
    {exodm_db:account_id_key(AID), exodm_db:encode_id(Name)}.


to_binary(A) when is_atom(A) ->
    atom_to_binary(A, latin1);
to_binary(L) when is_list(L) ->
    list_to_binary(L);
to_binary(B) when is_binary(B) ->
    B.

