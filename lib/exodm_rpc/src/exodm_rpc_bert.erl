-module(exodm_rpc_bert).

-export([dispatch/6,
	 request_timeout/1]).

-include_lib("lager/include/log.hrl").

dispatch(<<"to_device">>, Req, Env, AID, DID, Pid) ->
    ?debug("~p:dispatch(~p, ~p, ..., ~p)~n", [?MODULE, Req, Env, Pid]),
    case Req of
	{call, M, <<"push-config-set">>, [{'name', Cfg, _},
					  {'reference', Ref, _}]} ->
	    exodm_db:in_transaction(
	      fun(_Db) ->
		      case exodm_db_config:get_cached(AID, Cfg, Ref, DID) of
			  {ok, Values} ->
			      case bert_rpc(
				     Pid, exoport_config, push_config_set,
				     [[{module, M},
				       {name, Cfg},
				       {yang, <<"exodm.yang">>},
				       {values, Values}]], Req, Env, AID, DID) of
				  ok ->
				      remove_cached(AID, Cfg, Ref, DID)
			      end;
			  _ ->
			      error
		      end
	      end);
	{call, M, F, As} ->
	    bert_rpc(Pid, M, F, As,
		     Req, Env, AID, DID)
    end.

remove_cached(AID, Cfg, Ref, DID) ->
    case exodm_db_config:remove_cached(AID, Cfg, Ref, DID) of
	{ok, true} ->
	    exodm_db_config:switch_to_installed(AID, Cfg),
	    ok;
	{ok, false} ->
	    ok;
	Other ->
	    Other
    end.

request_timeout({_, _Env, {request, _, {call, _M, _Req, _}}}) ->
    ok.

bert_rpc(Pid, M, F, As, Req, Env, AID, DID) ->
    try
	dbg:tracer(),
	dbg:tpl(bert_rpc_exec,x),
	dbg:p(all,[c]),
    Result = (catch nice_bert_rpc:call(Pid, M, F, [remove_yang_info(As)])),
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
    after
	dbg:ctpl(bert_rpc_exec),
	dbg:stop()
    end.

remove_yang_info([As]) when is_list(As) ->
    [remove_yang_info(As)];
remove_yang_info(As) when is_list(As) ->
    yang_json:remove_yang_info(As).
