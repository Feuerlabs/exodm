-module(exodm_plugin).

-export([add_http_session/0,
	 login/2,
	 logout/0, logout/1,
	 get_account/0]).

-export([get_cached_config/3]).

-export([device_exists/1]).

add_http_session() ->
    exodm_http:add_session().

%% This call will activate a subscription on user delete events.
%% If `User' is deleted, the current process will receive a message
%% `{exodm_db_user, delete, AnyUser}' (note: not just for the current user).
%% This allows the process to stop performing operations that rely on the
%% presence of `User'.
login(Account, User) ->
    login_(Account, User, 3).

%% By default, this call will activate a subscription on user add events.
%% I.e. if a user is added/deleted, the current process will receive messages
%% of the form {exodm_db_user, add, AnyUser} (note: not just for the wanted
%% user). This allows processes to be started before the actual user has been
%% created, and then automatically pick up the user creation event and log in.
logout() ->
    logout(true).

logout(Resubscribe) when is_boolean(Resubscribe) ->
    exodm_db_session:logout(),
    exodm_db_user:unsubscribe(delete),
    if Resubscribe ->
	    catch exodm_db_user:subscribe(add);  % crashes if called repeatedly
       true ->
	    ok
    end,
    ok.

get_account() ->
    exodm_db_session:get_aid().

get_cached_config(ConfigSet, Ref, DeviceID) ->
    AID = exodm_db_session:get_aid(),
    exodm_db_config:get_cached(AID, ConfigSet, Ref, DeviceID).

device_exists(DID) ->
    exodm_db_device:exist(exodm_db_session:get_aid(), DID).


login_(Account, User, Retries) when is_integer(Retries) ->
    case get_account_id(Account) of
	AID when is_binary(AID) ->
	    case exodm_db_session:set_auth_as_user(
		   AID, User, kvdb_conf, _Sticky=true) of
		false when Retries > 0 ->
		    timer:sleep(500),
		    login_(Account, User, Retries-1);
		false ->
		    false;
		{true,_,_} ->
		    exodm_db_user:unsubscribe(add),
		    catch exodm_db_user:subscribe(delete),
		    exodm_db_session:set_trusted_proc(),
		    true
	    end;
	false ->
	    false
    end.

get_account_id(Acct) ->
    case exodm_db_account:exist(Acct) of
	false ->
	    case exodm_db_account:lookup_by_name(Acct) of
		AID when is_binary(AID) ->
		    AID;
		false ->
		    false
	    end;
	true ->
	    Acct
    end.
