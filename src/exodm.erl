-module(exodm).

-export([read_config_data/1,
	 list_groups/0]).

-spec ping() -> pong.
ping() ->
    pong.

-spec read_config_data(binary()) -> kvdb_conf:config_set().

read_config_data(Name) ->
    t(fun(AID, Db) ->
	      exodm_db_config:read_config_data(AID, Name)
      end).

list_groups() ->
    t(fun(AID, Db) ->
	      GIDs = exodm_db_group:list_group_keys(AID),
	      [exodm_db_group:lookup(AID, GID) || GID <- GIDs]
      end).




t(F) ->
    AID = exodm_db_session:get_aid(),
    exodm_db:in_transaction(fun(Db) -> F(AID, Db) end).
