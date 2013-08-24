-module(exodm_rpc_bert).

-export([dispatch/6,           % queued RPC
	 json_rpc/2,           % direct RPC
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

json_rpc({call, M, F, As} = Req, Env) ->
    ?debug("~p:json_rpc(~p, ~p)~n", [?MODULE, Req, Env]),
    {AID, DID} = {get_env('aid', Env), get_env('device-id', Env)},
    case exodm_rpc_handler:find_device_session(AID, DID, <<"exodm_bert">>) of
	{ok, Pid} ->
	    case bert_rpc_(Pid, M, F, As, Req, Env, AID, DID) of
		{reply, Res, _} ->
		    {ok, [{<<"rpc-status-string">>, <<"complete">>},
			  {<<"final">>, true}|Res]};
		Other ->
		    Other
	    end;
	_ ->
	    {ok, [{<<"rpc-status-string">>, <<"device-error">>},
		  {<<"final">>, true}]}
    end.

get_env(K, Env) ->
    case lists:keyfind(K, 1, Env) of
	false ->
	    error({required, K});
	{_, V} ->
	    V
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
    case bert_rpc_(Pid, M, F, As, Req, Env, AID, DID) of
	{reply, ok, _} ->
	    ok;
	_Other ->
	    error
    end.

bert_rpc_(Pid, M, F, As, Req, Env, AID, DID) ->
    Result = (catch nice_bert_rpc:call(Pid, M, F, [remove_yang_info(As)])),
    ?debug("RPC Result = ~p~n", [Result]),
    case Result of
	{reply, {notify, Method, Elems}, _} ->
	    exodm_rpc_handler:notification(
	      Method, Elems, Env, Req, AID, DID),
	    {reply, ok, []};
	Other ->
	    Other
    end.



remove_yang_info([As]) when is_list(As) ->
    [remove_yang_info(As)];
remove_yang_info(As) when is_list(As) ->
    yang_json:remove_yang_info(As).
