-module(exodm_db_system).

-export([init/0]).

-export([new_uid/0, set_uid_user/2, get_uid_user/1]).
-export([new_gid/0, set_gid_user/2, get_gid_user/1]).
-export([new_aid/0, set_aid_user/2, get_aid_user/1]).
-export([new_did/0, set_did_acct/2, get_did_acct/1]).


init() ->
    case exodm_db:read(ctr(<<"uid">>)) of
	{error, not_found} ->
	    exodm_db:write(ctr(<<"uid">>), <<0:32>>),
	    exodm_db:write(ctr(<<"gid">>), <<0:32>>),
	    exodm_db:write(ctr(<<"did">>), <<0:32>>),
	    exodm_db:write(ctr(<<"aid">>), <<0:32>>);
	_ ->
	    %% already initalized
	    ok
    end.

new_uid() -> exodm_db:user_id_key(new_id(<<"uid">>)).
new_gid() -> exodm_db:group_id_key(new_id(<<"gid">>)).
new_aid() -> exodm_db:account_id_key(new_id(<<"aid">>)).
new_did() -> exodm_db:device_id_key(new_id(<<"did">>)).

new_id(Type) ->
    %% If an ID has been hand-picked, we must skip past it.
    case get_id_user(Type, ID = exodm_db:update_counter(ctr(Type), 1)) of
	{error, not_found} ->
	    ID;
	_ ->
	    new_id(Type)
    end.

set_uid_user(ID, Name) ->
    set_id_user(<<"uid">>, exodm_db:user_id_key(ID), Name).
set_gid_user(ID, Name) ->
    set_id_user(<<"gid">>, exodm_db:group_id_key(ID), Name).
set_aid_user(ID, Name) ->
    set_id_user(<<"aid">>, exodm_db:account_id_key(ID), Name).
set_did_acct(ID, Name) ->
    set_id_user(<<"did">>, exodm_db:device_id_key(ID), Name).

set_id_user(Type, ID, Name) ->
    exodm_db:write(
      exodm_db:kvdb_key_join([<<"system">>,Type,ID]), Name).

get_uid_user(ID) -> get_id_user(<<"uid">>, exodm_db:user_id_key(ID)).
get_gid_user(ID) -> get_id_user(<<"gid">>, exodm_db:group_id_key(ID)).
get_aid_user(ID) -> get_id_user(<<"aid">>, exodm_db:account_id_key(ID)).
get_did_acct(ID) -> get_id_user(<<"did">>, exodm_db:device_id_key(ID)).

get_id_user(Type, ID) ->
    exodm_db:read(
      exodm_db:kvdb_key_join([<<"system">>,Type,ID])).

ctr(Type) -> key(<<"last_", Type/binary>>).

key(Item) ->
    exodm_db:kvdb_key_join(<<"system">>, exodm_db:to_binary(Item)).
