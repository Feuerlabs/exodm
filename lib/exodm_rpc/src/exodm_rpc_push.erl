-module(exodm_rpc_push).

-compile(export_all).

notify(AID, DID) ->
    case exodm_db_device:lookup_attr(AID, DID, notification_sent) of
	[{_, TS}] when TS =/= <<>> ->
	    ok;
	_ ->
	    case exodm_db_device:push_protocol(AID, DID) of
		none ->
		    ok;
		Protocol when is_binary(Protocol) ->
		    case push(AID, DID, Protocol, messages) of
			{ok, _} -> notify_sent(AID, DID);
			Other ->
			    Other
		    end
	    end
    end.

clear_notify_sent(ExtID) ->
    {AID, DID} = exodm_db_device:dec_ext_key(ExtID),
    clear_notify_sent(AID, DID).

clear_notify_sent(AID, DID) ->
    write_notification_sent(AID, DID, <<>>).

notify_sent(AID, DID) ->
    {{Y,Mo,D},{H,Mi,S}} = erlang:universaltime(),
    TS = lists:concat([i2l(Y) | [i2(X) || X <- [Mo,D,H,Mi,S]]]),
    write_notification_sent(AID, DID, TS).

write_notification_sent(AID, DID, TS) ->
    case exodm_db_device:exist(AID, DID) of
	true ->
	    exodm_db_device:write_attrs(AID, DID,
					[{notification_sent, TS}]);
	false ->
	    error(device_not_found)
    end.

i2(I) when I >= 0, I < 10 ->
    "0" ++ integer_to_list(I);
i2(I) when I >= 10, I =< 99 ->
    integer_to_list(I).

i2l(I) ->
    integer_to_list(I).


add_active_channel(AID, DID) ->
    exodm_rpc_handler:add_device_session(AID, DID, push).

remove_active_channel(AID, DID) ->
    exodm_rpc_handler:rm_device_session(AID, DID, push).

active_channel(AID, DID) ->
    exodm_rpc_handler:find_device_session(AID, DID, push).

push(AID, DID, Protocol, Message) ->
    case get_channel(AID, DID, Protocol) of
	{ok, Ch} ->
	    Mod = exodm_rpc_protocol:module(Protocol),
	    case Mod:encode_push_message(AID, DID, Message) of
		{ok, EncMsg} ->
		    Mod:send(Ch, AID, DID, Protocol, EncMsg);
		ignore ->
		    ok;
		error ->
		    error
	    end;
	error ->
	    ok
    end.

get_channel(AID, DID, Protocol) ->
    case active_channel(AID, DID) of
	{ok, _} = Res -> Res;
	error ->
	    case open_on_demand(Protocol) of
		true ->
		    open_push_channel(AID, DID, Protocol);
		false ->
		    error
	    end
    end.

-spec open_on_demand(_Protocol::binary()) -> boolean().
open_on_demand(_) ->
    false.

%% If we're using a push server (JSON RPC), we don't need to open and maintain
%% a session. If we're talking directly to e.g. ASPN, we need to keep a
%% channel open, since ASPNs will interpret rapid or frequent opening and
%% closing of channels as a Denial of Service attack.
open_push_channel(_AID, _DID, _Protocol) ->
    error.
