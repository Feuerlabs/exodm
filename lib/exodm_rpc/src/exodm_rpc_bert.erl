-module(exodm_rpc_bert).

-export([dispatch/5]).

-include_lib("lager/include/log.hrl").

dispatch(Req, Env, AID, DID, Pid) ->
    ?debug("~p:dispatch(~p, ~p, ..., ~p)~n", [?MODULE, Req, Env, Pid]),
    case Req of
	{request, _, {call, M, F, As}} ->
	    Result = (catch nice_bert_rpc:call(Pid, M, F, [As])),
	    ?debug("RPC Result = ~p~n", [Result]),
	    case Result of
		{reply, {notify, Method, Elems}, _} ->
		    exodm_rpc_handler:notification(
		      Method, Elems, Env, Req, AID, DID);
		{reply, ok, _} ->
		    ok;
		_Other ->
		    error
	    end
    end.
