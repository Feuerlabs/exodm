-module(exodm_rpc_bert_challenge).

-export([authenticate/3,
	 incoming/2,
	 outgoing/2]).
-export([keys/1]).

-include_lib("lager/include/log.hrl").
-define(dbg(F,A), ?debug("~p " ++ F, [self()|A])).

authenticate(S, Role, Arg) ->
    ?dbg("~p:authenticate(...)~n", [?MODULE]),
    try bert_challenge:authenticate(S, Role, Arg) of
	{ok, St} ->
	    ID0 = bert_challenge:remote_id(St),
	    ID = normalize_ext_id(ID0),
	    %% Do we want the device to be authenticated as a certain
	    %% Exodm user? Perhaps this should be optional?
	    exodm_db_session:set_auth_as_device(ID),

	    %% Register a property. Why not a (unique) name?
	    %% One thinks there should only be one active session at any
	    %% given time, but this is only true if we have a reusable session.
	    %% TODO: think this through.
	    ?dbg("remote ID = ~p~n", [ID]),
	    exodm_rpc_handler:add_device_session(ID, <<"exodm_bert">>),
	    gproc:reg(_Prop = {p,l,{exodm_rpc, active_device, ID}}),
	    ?dbg("regged with gproc: ~p~n", [_Prop]),
	    exodm_rpc_dispatcher:check_queue(<<"to_device">>, ID),
	    ?dbg("queue checked~n", []),
	    {ok, St};
	error ->
	    error
    catch
	error:E ->
	    ?dbg("Caught exception ~p~n"
		 "Trace = ~p~n", [E, erlang:get_stacktrace()]),
	    {error, E}
    end.

normalize_ext_id(ID) ->
    {AID, DID} = exodm_db_device:dec_ext_id(ID),
    exodm_db_device:enc_ext_id(AID, DID).

outgoing(Data, St) ->
    bert_challenge:outgoing(Data, St).

incoming(Data, St) ->
    bert_challenge:incoming(Data, St).

%% @spec keys(ID) -> {MyKey, TheirKey} | error
%% @doc dynamic key extraction callback
%% @end
%%
keys(ID) ->
    R = case exodm_db_device:dec_ext_key(ID) of
	    {AID, DID} ->
		case exodm_db_device:lookup_attr(AID, DID, 'server-key') of
		    [{_, Sk}] ->
			case exodm_db_device:lookup_attr(
			       AID, DID, 'device-key') of
			    [{_, Ck}] -> {Sk, Ck};
			    _ -> error
			end;
		    _ -> error
		end;
	    error -> error
	end,
    ?debug("~p:keys(~p) -> ~p~n", [?MODULE, ID, R]),
    R.
