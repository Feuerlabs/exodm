-module(exodm_rpc_bert_challenge).

-export([authenticate/3,
	 incoming/2,
	 outgoing/2]).
-export([keys/1]).

-include("log.hrl").
-define(dbg(F,A), ?debug("~p " ++ F, [self()|A])).

authenticate(S, Role, Arg) ->
    ?dbg("~p:authenticate(~p, ~p, ~p)~n", [?MODULE, S, Role, Arg]),
    jobs:ask(exodm_rpc_bert_sessions),
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
	    exodm_rpc_handler:add_device_session(
	      ID, Protocol = <<"exodm_bert">>),
	    prune_device_sessions(ID, Protocol),
	    gproc:reg(_Prop = {p,l,{exodm_rpc, active_device, ID}}),
	    ?dbg("regged with gproc: ~p~n", [_Prop]),
	    exodm_rpc_dispatcher:check_queue(<<"to_device">>, ID),
	    ?dbg("queue checked~n", []),
	    exodm_rpc_push:clear_notify_sent(ID),
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
    {AID, DID} = exodm_db_device:dec_ext_key(ID),
    exodm_db_device:enc_ext_key(AID, DID).

%% If there are lingering sessions (perhaps with sockets in TIME_WAIT), we
%% don't want to keep them around. There should only be one active session.
%% (Note that we do this for BERT specifically; it may be a generic pattern).
prune_device_sessions(ID, Protocol) ->
    Sessions = exodm_rpc_handler:device_sessions(ID),
    prune_sessions_(Sessions, Protocol).

prune_sessions_([{Pid, Protocol} = Me|Rest], Protocol) when Pid == self() ->
    Keep = lists:filter(
	     fun({P, Prot}) when Prot == Protocol ->
		     ?dbg("Killing old device session ~p~n", [P]),
		     exit(P, replaced),
		     false;
		(_) ->
		     true
	     end, Rest),
    [Me | Keep];
prune_sessions_([{_,Prot} = H|T], Protocol) when Prot =/= Protocol ->
    [H | prune_sessions_(T, Protocol)];
prune_sessions_([], _) ->
    [].


outgoing(Data, St) ->
    bert_challenge:outgoing(Data, St).

incoming(Data, St) ->
    bert_challenge:incoming(Data, St).

%% @spec keys(ID) -> {MyKey, TheirKey} | error
%% @doc dynamic key extraction callback
%% @end
%%
keys(ID) ->
    ?dbg("keys(~p)", [ID]),
    R = case exodm_db_device:dec_ext_key(ID) of
	    {AID, DID} ->
		?dbg("keys: AID ~p, DID ~p~n", [AID, DID]),
		case exodm_db_device:lookup_attr(AID, DID, 'server-key') of
		    [{_, Sk}] ->
			?dbg("keys: server key found.~n", []),
			case exodm_db_device:lookup_attr(
			       AID, DID, 'device-key') of
			    [{_, Ck}] -> 
				?dbg("keys: device key found.~n", []),
				{Sk, Ck};
			    _ -> error
			end;
		    _ -> error
		end;
	    error -> error
	end,
    ?dbg("keys: result ~p", [R]),
    R.
