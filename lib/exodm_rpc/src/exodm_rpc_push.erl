-module(exodm_rpc_push).

-compile(export_all).


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
		    send_on_channel(Ch, AID, DID, Protocol, EncMsg);
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

send_on_channel(_Ch, _AID, _DID, _Protocol, _EncMsg) ->
    ok.
