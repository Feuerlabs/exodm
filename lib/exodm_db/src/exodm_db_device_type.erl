-module(exodm_db_device_type).

-export([init/1]).
-export([
	 new/3,
	 update/3,
	 delete/2,
	 exist/2,
	 add_device/3,
	 remove_device/3,
	 list_next/3,
	 list_devices/4
	]).

init(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:in_transaction(
      fun(_) ->
	      kvdb_conf:add_table(table(AID), [])
      end).

table(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    <<AID/binary, "_devtype">>.

new(AID0, Name0, Options) ->
    AID = exodm_db:account_id_key(AID0),
    Name = exodm_db:encode_id(Name0),
    true = exodm_db:all_required([protocol], Options),
    ok.

update(AID0, Name0, Options) ->
    error(nyi).

delete(AID0, Name0) ->
    error(nyi).

exist(AID0, Name0) ->
    {AID,Name} = encode(AID0, Name0),
    case exodm_db:read(table(AID), Name) of
	{error, not_found} ->
	    false;
	{ok, _} ->
	    true
    end.

add_device(AID0, Name0, DID0) ->
    error(nyi).

remove_device(AID0, Name0, DID0) ->
    error(nyi).

list_next(AID0, N, Prev) ->
    error(nyi).

list_devices(AID0, Name0, N, Prev) ->
    error(nyi).

encode(AID, Name) ->
    {exodm_db:account_id_key(AID), exodm_db:encode_id(Name)}.
