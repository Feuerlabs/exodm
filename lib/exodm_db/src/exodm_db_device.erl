%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense device manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_device).

-export([new/3, update/3, lookup/2, lookup/1, exist/2, exist/1]).

-import(exodm_db, [write/2, binary_opt/2, to_binary/1]).

%%
%% /u<UID>/devices/x<DID>/name    = <DeviceName>
%% /u<UID>/devices/x<DID>/msisdn  = <MSISDN>
%% /u<UID>/devices/x<DID>/imsi    = <IMSI>
%% /u<UID>/devices/x<DID>/imei    = <IMEI>
%% /u<UID>/devices/x<DID>/__ck      = <device-key>  64-bit
%% /u<UID>/devices/x<DID>/__sk      = <server-key>  64-bit
%%

%% FIXME option validation

new(UID, DID, Options) ->
    Key = key(UID, DID),
    insert(Key,name,     binary_opt(name, Options)),
    insert(Key,msisdn,   binary_opt(msisdn,Options)),
    insert(Key,imsi,     binary_opt(imsi,Options)),
    insert(Key,imei,     binary_opt(imei,Options)),
    insert(Key,activity, binary_opt(activity,Options)),
    %% __ck, __sk are device keys and need special attention 
    insert(Key,'__ck',     binary_opt('__ck',Options)),
    insert(Key, '__sk',    binary_opt('__sk',Options)),
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
	  ({activity,Value}) ->
	      insert(Key,activity,to_binary(Value));
	  ({'__ck',Value}) when is_integer(Value) ->
	      insert(Key,'__ck', <<Value:64>>);
	  ({'__sk',Value}) when is_integer(Value) ->
	      insert(Key,'__sk', <<Value:64>>);
	  ({'__ck',Value}) when is_binary(Value), byte_size(Value) =:= 8 ->
	      insert(Key,'__ck', Value);
	  ({'__sk',Value}) when is_binary(Value), byte_size(Value) =:= 8 ->
	      insert(Key,'__sk', Value)
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
	read(Key, activity) ++
	read(Key, status).

exist(UID, DID) ->
    exist(key(UID, DID)).

exist(Key) ->
    case exodm_db:read(Key) of
	{ok, _} -> true;
	{error, not_found} -> false
    end.


%% utils

key(UID, DID) ->
    U = exodm_db:user_id_key(UID),
    D = exodm_db:device_id_key(DID),
    exodm_db:kvdb_key_join([U, <<"devices">>, D]).

insert(Key, Item, Value) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    exodm_db:write(Key1, Value).

read(Key,Item) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    case exodm_db:read(Key1) of
	{ok,{_,_,Value}} -> [{Item,Value}];
	{error,not_found} -> []
    end.
