-module(exodm_db_system).

-export([init/0]).

-export([new_gid/0, set_gid_user/2, get_gid_user/1]).
-export([new_aid/0, set_aid_user/2, get_aid_user/1]).
-export([new_did/0, set_did_acct/2, get_did_acct/1]).


init() ->
    case exodm_db:read(ctr(<<"aid">>)) of
	{error, not_found} ->
	    exodm_db:write(ctr(<<"gid">>), <<0:32>>),
	    exodm_db:write(ctr(<<"did">>), <<0:32>>),
	    exodm_db:write(ctr(<<"aid">>), <<0:32>>);
	_ ->
	    %% already initalized
	    ok
    end.

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

set_gid_user(ID, Name) ->
    set_id_user(<<"gid">>, exodm_db:group_id_key(ID), Name).
set_aid_user(ID, Name) ->
    set_id_user(<<"aid">>, exodm_db:account_id_key(ID), Name).
set_did_acct(ID, Name) ->
    set_id_user(<<"did">>, exodm_db:device_id_key(ID), Name).

set_id_user(Type, ID, Name) ->
    exodm_db:write(key(Type,ID), Name).

get_gid_user(ID) -> get_id_user(<<"gid">>, ID).
get_aid_user(ID) -> get_id_user(<<"aid">>, ID).
get_did_acct(ID) -> get_id_user(<<"did">>, ID).


id_key(<<"gid">>, ID) -> exodm_db:group_id_key(ID);
id_key(<<"aid">>, ID) -> exodm_db:account_id_key(ID);
id_key(<<"did">>, ID) -> exodm_db:device_id_key(ID).

get_id_user(Type, ID) ->
    exodm_db:read(key(Type, id_key(Type, ID))).

ctr(Type) -> key(<<"last_", Type/binary>>).

key(Item) ->
    exodm_db:join_key(<<"system">>, exodm_db:to_binary(Item)).

key(Type, Item) ->
    exodm_db:join_key([<<"system">>, Type, Item]).
