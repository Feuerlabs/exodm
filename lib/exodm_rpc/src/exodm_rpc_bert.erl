-module(exodm_rpc_bert).

-export([dispatch/5]).

-include_lib("lager/include/log.hrl").

dispatch(Req, Env, AID, DID, Pid) ->
    ?debug("~p:dispatch(~p, ~p, ..., ~p)~n", [?MODULE, Req, Env, Pid]),
    case Req of
	{request, _, {call, M, 'push-config-data', [{'config-data', Cfg},
						    {'reference', Ref}]}} ->
	    exodm_db:in_transaction(
	      fun(_Db) ->
		      case exodm_db_config:get_cached(AID, Cfg, Ref, DID) of
			  {ok, Values} ->
			      bert_rpc(Pid, exoport_config, push_config_data,
				       [[{module, M},
					 {config_data, Cfg},
					 {values, Values}]], Req, Env, AID, DID);
			  _ ->
			      error
		      end
	      end);
	{request, _, {call, M, F, As}} ->
	    bert_rpc(Pid, M, F, [As], Req, Env, AID, DID)
    end.

bert_rpc(Pid, M, F, As, Req, Env, AID, DID) ->
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
    end.
