-module(exodm_rpc_exoport_http).

-export([dispatch/6,
	 request_timeout/1]).

-export([recv/3]).

-include_lib("lager/include/log.hrl").

dispatch(<<"to_device">>, Req, Env, AID, DID, Pid) ->
    ?debug("~p:dispatch(~p, ~p, ..., ~p)~n", [?MODULE, Req, Env, Pid]),
    Msg =
	case Req of
	    {call, M, 'push-config-set', [{'name', Cfg},
					  {'reference', Ref}]} ->
		exodm_db:in_transaction(
		  fun(_Db) ->
			  case exodm_db_config:get_cached(AID, Cfg, Ref, DID) of
			      {ok, Values} ->
				  {struct,
				   [{"class", "call"},
				    {"method", str(M) ++ ":push_config_set"},
				    {"params", {struct,
						[{"name", Cfg},
						 {"values", Values}]}}
				   ]};
			  _ ->
			      error
		      end
	      end);
	    {call, M, F, As} ->
		{struct, [{"class", "call"},
			  {"method", str(M) ++ ":" ++ str(F)},
			  {"params", As}]}
	end,
    call(Pid, {msg, Msg}).

request_timeout(_) ->
    ok.

recv(ExtID, N, Timeout) when is_integer(N), N > 0 ->
    exodm_rpc_handler:add_device_session(ExtID, <<"exoport_http">>),
    TRef = start_timer(Timeout),
    Me = self(),
    Pid = spawn(fun() ->
			check_queue(ExtID, Me)
		end),
    try recv_(N, TRef, Pid, [])
    after
	exodm_rpc_handler:rm_device_session(ExtID, <<"exoport_http">>)
    end.

start_timer(T) ->
    erlang:start_timer(T, self(), timeout).

cancel_timer(Ref) ->
    erlang:cancel_timer(Ref).

recv_(0, TRef, _QPid, Acc) ->
    cancel_timer(TRef),
    lists:reverse(Acc);
recv_(N, TRef, QPid, Acc) ->
    receive
	{call, {Pid, Ref}, {msg, M}} ->
	    Pid ! {Ref, ok},
	    recv_(N-1, TRef, QPid, [M | Acc]);
	%% {Pid, exodm_rpc_dispatcher, done} ->
	%%     cancel_timer(TRef),
	%%     lists:reverse(Acc);
	{QPid, Result} when Result == done; Result == error ->
	    %% Perhaps we should do something wise if Result == error? FIXME
	    cancel_timer(TRef),
	    lists:reverse(Acc);
	{timeout, TRef, _} ->
	    lists:reverse(Acc)
    end.

call(Pid, Req) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {call, {self(), Ref}, Req},
    receive
	{Ref, Reply} ->
	    Reply;
	{'DOWN', Ref, _, _, Reason} ->
	    ?debug("Session pid ~p is DOWN: ~p~n", [Pid, Reason]),
	    error
    after 5000 ->
	    ?error("Timeout waiting for session pid ~p~n", [Pid]),
	    error
    end.

str(A) when is_atom(A)   -> atom_to_list(A);
str(L) when is_list(L)   -> L;
str(B) when is_binary(B) -> binary_to_list(B).

check_queue(ExtID, Parent) ->
    MRef = erlang:monitor(process, Parent),
    exodm_rpc_dispatcher:attempt_dispatch(
      kvdb_conf:instance(), to_device, ExtID, _Reply = true),
    receive
	{_, exodm_rpc_dispatcher, done} ->
	    Parent ! {self(), done};
	{'DOWN', MRef, _, _} ->
	    done
    after 5000 ->
	    %% shouldn't happen
	    Parent ! {self(), error}
    end.
