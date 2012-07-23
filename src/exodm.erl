-module(exodm).

-export([ping/0,
	 read_config_data/1,
	 add_config_data_members/2,
	 list_groups/0,
	 push_config/1]).

-include_lib("kvdb/include/kvdb_conf.hrl").

-spec ping() -> pong.
ping() ->
    pong.

-spec read_config_data(binary()) -> kvdb_conf:config_set().

read_config_data(Name) ->
    t(fun(AID, _Db) ->
	      exodm_db_config:read_config_data(AID, Name)
      end).

add_config_data_members(Name, DeviceIDs) ->
    t(fun(AID, _Db) ->
	      exodm_db_config:add_config_data_members(AID, Name, DeviceIDs)
      end).

push_config(Name) ->
    t(fun(AID, _Db) ->
	      case exodm_db_config:list_config_data_values(AID, Name) of
		  {error, _} = E ->
		      error(E);
		  #conf_tree{} = Values ->
		      Devices = exodm_db_config:list_config_data_members(
				  AID, Name),
		      [queue_push_data_request(D, Values) || D <- Devices],
		      ok
	      end
      end).

list_groups() ->
    t(fun(AID, _Db) ->
	      GIDs = exodm_db_group:list_group_keys(AID),
	      [exodm_db_group:lookup(AID, GID) || GID <- GIDs]
      end).

queue_push_data_request(_DID, _Values) ->
    error(nyi).


t(F) ->
    AID = exodm_db_session:get_aid(),
    exodm_db:in_transaction(fun(Db) -> F(AID, Db) end).
