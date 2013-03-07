-module(exodm_plugin).

-export([add_http_session/0,
	 login/2, login/3,
	 logout/0, logout/1,
	 get_account/0]).

-export([notification/4,
	 queue_notification/4,
	 queue_reverse_request/4,
	 check_queue/2]).

-export([get_cached_config/3]).

-export([device_exists/1,
	 lookup_device_position/1,
	 lookup_device_keys/1,
	 add_device_session/2,
	 remove_device_session/2]).

-include_lib("lager/include/log.hrl").

add_http_session() ->
    exodm_http:add_session().

%% This call will activate a subscription on user delete events.
%% If `User' is deleted, the current process will receive a message
%% `{exodm_db_user, delete, AnyUser}' (note: not just for the current user).
%% This allows the process to stop performing operations that rely on the
%% presence of `User'.
login(Account, User) ->
    login(Account, User, true).

login(Account, User, Subscribe) ->
    login_(Account, User, Subscribe, 3).

%% By default, this call will activate a subscription on user add events.
%% I.e. if a user is added/deleted, the current process will receive messages
%% of the form {exodm_db_user, add, AnyUser} (note: not just for the wanted
%% user). This allows processes to be started before the actual user has been
%% created, and then automatically pick up the user creation event and log in.
logout() ->
    logout(true).

logout(Resubscribe) when is_boolean(Resubscribe) ->
    exodm_db_session:logout(),
    exodm_db_account:unsubscribe(delete),
    if Resubscribe ->
	    catch exodm_db_account:subscribe(add);  % crashes if called repeatedly
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

lookup_device_position(DeviceID) ->
    exodm_db_device:lookup_position(get_account(), DeviceID).

lookup_device_keys(DeviceID) ->
    exodm_db_device:lookup_device_keys(get_account(), DeviceID).

add_device_session(DeviceID, Protocol) ->
    ?debug("add_device_session(~p, ~p)~n", [DeviceID, Protocol]),
    exodm_rpc_handler:add_device_session(get_account(), DeviceID, Protocol).

remove_device_session(DeviceID, Protocol) ->
    ?debug("remove_device_session(~p, ~p)~n", [DeviceID, Protocol]),
    exodm_rpc_handler:rm_device_session(get_account(), DeviceID, Protocol).

check_queue(Direction, DeviceID0) when Direction==to_device;
				       Direction==from_device ->
    DeviceID = exodm_db:encode_id(DeviceID0),
    ?debug("check_queue(~p, ~p)~n", [Direction, DeviceID]),
    ExtID = exodm_db:enc_ext_key(get_account(), DeviceID),
    exodm_rpc_dispatcher:check_queue(Direction, ExtID).

notification(Method, Elems, Env, DeviceID) ->
    ?debug("notification(~p, ~p, ~p, ~p)~n", [Method, Elems, Env, DeviceID]),
    AID = get_account(),
    case exodm_db_device:exist(AID, DeviceID) of
	true ->
	    exodm_rpc_handler:notification(Method, Elems, Env, AID, DeviceID);
	false ->
	    ?debug("no such device (~p, ~p)~n", [AID, DeviceID]),
	    error(unknown_device)
    end.

queue_notification(Module, Method, Elems, Env) when is_list(Elems),
						    is_list(Env) ->
    ?debug("(~p, ~p, ~p, ~p)~n", [Module, Method, Elems, Env]),
    exodm_rpc:queue_notification(Module, notify, Env, Method, Elems).

queue_reverse_request(Module, Method, Elems, Env) ->
    exodm_rpc:queue_notification(Module, reverse_request, Env, Method, Elems).

login_(Account, User, Subscribe, Retries) when is_integer(Retries) ->
    case get_account_id(Account) of
	AID when is_binary(AID) ->
	    case exodm_db_session:set_auth_as_user(
		   AID, User, kvdb_conf, _Sticky=true) of
		false when Retries > 0 ->
		    timer:sleep(500),
		    login_(Account, User, Subscribe, Retries-1);
		false ->
		    false;
		true ->
		    exodm_db_session:set_trusted_proc(),
		    if Subscribe ->
			    exodm_db_account:unsubscribe(add),
			    catch exodm_db_account:subscribe(delete),
			    true;
		       true ->
			    true
		    end
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
