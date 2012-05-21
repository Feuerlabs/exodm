-module(exodm_db_system).

-export([init/0]).

-export([new_uid/0, set_uid_user/2]).


init() ->
    exodm_db:write(key(last_uid), <<0:32>>).

new_uid() ->
    exodm_db:update_counter(key(last_uid), 1).

set_uid_user(UID, UName) ->
    exodm_db:write(
      exodm_db:kvdb_key_join([<<"system">>,<<"uid">>,UID]), UName).

key(Item) ->
    exodm_db:kvdb_key_join(<<"system">>, exodm_db:to_binary(Item)).
