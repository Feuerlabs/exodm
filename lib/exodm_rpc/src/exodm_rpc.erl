-module(exodm_rpc).
-behaviour(gen_server).

-compile(export_all).
-export([handler_session/1]).
%% -export([load_specs/0, reload_specs/0]).
-export([notification/6,
	 queue_message/5,
	 queue_notification/5]).

-export([start_link/0]).
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).
-export([to_json_/4]). % for testing
-export([ping/0,
	 notification/3]).
-export([std_specs/0]).

-include_lib("lager/include/log.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-record(st, {}).
-define(TAB, ?MODULE).
-define(TMP_TAB, exodm_rpc_tmp).

ping() ->
    pong.

notification(Module, Method, Elems) ->
    {did, DID} = exodm_db_session:get_user(),
    AID = exodm_db_session:get_aid(),
    Yang = <<(to_binary(Module))/binary, ".yang">>,
    exodm_rpc_handler:notification(
      Method, Elems, [{yang, Yang},
		      {'device-id', DID},
		      {aid, AID}], [], AID, DID).

queue_notification(Module, Type, Env0, Method, Elems) when
      Type == notify; Type == reverse_request ->
    User = exodm_db_session:get_user(),
    AID = exodm_db_session:get_aid(),
    Yang = <<(to_binary(Module))/binary, ".yang">>,
    Env = [{yang, Yang},
	   {user, User},
	   {aid, exodm_db:account_id_key(AID)}|Env0],
    exodm_db:in_transaction(
      fun(Db) ->
	      exodm_rpc_handler:queue_message(
		Db, AID, from_device, Env, {Type, Method, Elems})
      end).


to_binary(A) when is_atom(A) ->
    atom_to_binary(A, latin1);
to_binary(B) when is_binary(B) ->
    B;
to_binary(L) when is_list(L) ->
    list_to_binary(L).



%% @doc Handle a JSON-RPC request; once go-ahead is given from load control.
handler_session(Arg) ->
    jobs:run(
      exodm_rpc_from_web,
      fun() ->
	      try
		  ?debug("handler_session(~p)~n", [Arg]),
		  Peer = peername(Arg#arg.clisock),
		  {ok,{IP,_}} = Peer,
		  Arg2=Arg#arg{state = [{ip, IP}]},
		  yaws_rpc:handler_session(Arg2, {?MODULE, web_rpc})
	      catch
		  error:E ->
		      Trace = erlang:get_stacktrace(),
		      ?debug("handler_session crashed:~n"
			     "~p~n~p", [E, Trace]),
		      error({E, Trace})
	      end
      end).

peername({ssl, S})            -> ssl:peername(S);
peername({sslsocket,_,_} = S) -> ssl:peername(S);
peername(S)                   -> inet:peername(S).


web_rpc(St, Req, Session) ->
    try kvdb_conf:in_transaction(
	  fun(Db) ->
		  web_rpc_(Db, St, Req, Session)
	  end)
    catch
	error:E ->
	    ?error("*** ERROR! ~p:web_rpc(~p, ~p, ~p)~n"
		   "  E = ~p~n"
		   "  Trace = ~p~n",
		   [?MODULE, St, Req, Session, E, erlang:get_stacktrace()]),
	    error(E)
    end.

web_rpc_(Db, [{ip, _IP}], {call, Method, Request}, Session) ->
    ?debug("web_rpc: Method = ~p; Request = ~p~n", [Method, Request]),
    AID = exodm_db_session:get_aid(),
    UID = exodm_db_session:get_user(),
    Env0 = [{aid, AID},
	    {user, UID}],
    case json_get_device_id(Request) of
	{ok, DID} ->
	    ?debug("found device-id: ~p~n", [DID]),
	    case find_method_spec(Method, AID, DID) of
		{ok, Yang, Module, ShortMeth, Protocol, Spec} ->
		    ?info("Method spec (~p): ~p~n"
			   "Module = ~p~n", [Method, Spec, Module]),
		    Env1 = [{yang, Yang},
			    {device_id, DID},
			    {protocol, Protocol}|Env0],
		    case validate_request(
			   ShortMeth, Module, Request, Spec) of
			{verified, {request, Attrs, _} = RPC} ->
			    ?debug("request verified: ~p~n", [RPC]),
			    queue_message(
			      Db, AID, to_device, Env1, RPC),
			    Response = accept_response(Attrs ++ Env1, Spec),
			    {true, 0, Session, {response, Response}};
			{not_verified, Reason} ->
			    ?debug("request NOT verified: ~p~n", [Reason]),
			    Response = error_response(Reason),
			    {false, Response}
		    end;
		error -> false
	    end;
	error ->
	    %% Check if this is a general RPC, supported by one of the system
	    %% specs. Otherwise, it's an error.
	    ?debug("no device-id~n", []),
	    case is_exodm_method(Method, AID) of
		{ok, Yang, Module, ShortMeth, Protocol, Spec} ->
		    ?debug("ExoDM method spec (~p): ~p~n"
			   "Module = ~p; ShortM = ~p~n",
			   [Method, Spec, Module, ShortMeth]),
		    Env1 = [{yang, Yang},
			    {protocol, Protocol}|Env0],
		    case validate_request(
			   ShortMeth, Module, Request, Spec) of
			{verified, RPC} ->
			    ?debug("request verified: ~p~n", [RPC]),
			    handle_exodm_rpc(Protocol, Env1, RPC, Session, Spec);
			{not_verified, Reason} ->
			    ?debug("request NOT verified: ~p~n", [Reason]),
			    Response = error_response(Reason),
			    {false, Response}
		    end;
		error ->
		    ?debug("is_exodm_method(~p, ~p) -> error~n", [Method,AID]),
		    false
	    end
    end.

handle_exodm_rpc(Protocol, Env, RPC, Session, Spec) ->
    Mod = exodm_rpc_protocol:module(Protocol),
    ?debug("handle_exodm_rpc(~p, ~p, ~p, ~p, ~p)~n"
	   "Mod = ~p~n", [Protocol, Env, RPC, Session, Spec, Mod]),
    try Mod:json_rpc(RPC, Env) of
	{ok, Result} = _OK ->
	    ?debug("~p:json_rpc(...) -> ~p~n", [?MODULE, _OK]),
	    Resp = success_response(Result, Env, RPC, Spec),
	    ?debug("Resp = ~p~n", [Resp]),
	    {true, 0, Session, {response, Resp}};
	{error, Err} = _Error ->
	    ?debug("~p:json_rpc(...) -> ~p~n", [?MODULE, _Error]),
	    {false, error_response(convert_error(Err, RPC))}
    catch error:E ->
	    ?debug("~p:json_rpc(...) -> *ERROR*:~p~n",
		   [?MODULE, {E, erlang:get_stacktrace()}]),
	    {false, error_response(convert_error(E, RPC))}
    end.


notification(Method, Elems, Env, Req, AID, DID) ->
    ?debug("notification(~p, ~p, ~p, ~p, ~p, ~p)~n",
	   [Method, Elems, Env, Req, AID, DID]),
    {_, Yang} = lists:keyfind(yang, 1, Env),
    case exodm_db_yang:rpcs(Yang) of
	[] ->
	    error({no_rpcs, Yang});
	[{_, Spec}] ->
	    Key = binary_to_list(
		    <<(mod(Yang))/binary, ":",
		       (atom_to_binary(Method, latin1))/binary>>),
	    case lists:keyfind(Key, 1, Spec) of
		{_, {notification,_,{struct, SubSpec}}} ->
		    ?debug("SubSpec = ~p~n", [SubSpec]),
		    {_, Params} = lists:keyfind("params", 1, SubSpec),
		    NewParams = to_json(Params, Env, Elems),
		    JSON = {struct, lists:keyreplace("params", 1, SubSpec,
						     {"params", NewParams})},
		    ?debug("JSON = ~p~n", [JSON]),
		    post_json(Env, JSON)
	    end
    end,
    ok.

mod(Yang) ->
    Y = case Yang of
	    <<"system.", Rest/binary>> -> Rest;
	    <<"user."  , Rest/binary>> -> Rest;
	    _ -> Yang
	end,
    Sz = byte_size(Y),
    SzA = Sz-5,
    <<M:SzA/binary, ".yang">> = Y,
    M.


is_exodm_method(Method, AID) ->
    case binary:split(atom_to_binary(Method, latin1), <<":">>) of
	[MethodBin] ->
	    Mod = <<"exodm">>,
	    ?debug("Mod = ~p; MethodBin = ~p~n", [Mod, MethodBin]),
	    find_method_spec_(Mod, [<<"exodm">>], MethodBin);
	[Mod, MethodBin] ->
	    ?debug("Mod = ~p; MethodBin = ~p~n", [Mod, MethodBin]),
	    YangSpecs = annotate_specs(
			  std_specs() ++ exodm_db_account:system_specs(AID)),
	    find_method_spec_(Mod, YangSpecs, MethodBin)
    end.

std_specs() ->
    [<<"exodm">>].

json_get_device_id({struct, L}) ->
    case lists:keyfind("device-id", 1, L) of
	{_, ID} ->
	    {ok, list_to_binary(ID)};
	false ->
	    error
    end.

annotate_specs(Specs) ->
    [{<<>>, <<S/binary, ".yang">>, <<"exodm">>} || S <- Specs].

find_method_spec(Method, AID, DevID) ->
    ?debug("find_method_spec(~p, ~p)~n", [Method, DevID]),
    DID = exodm_db:encode_id(DevID),
    YangSpecs = exodm_db_device:yang_modules(AID, DID),
    case binary:split(atom_to_binary(Method, latin1), <<":">>) of
	[MethodBin] ->
	    Mod = get_default_module(AID, DID),
	    find_method_spec_(Mod, YangSpecs, MethodBin);
	[Mod, MethodBin] ->
	    find_method_spec_(Mod, YangSpecs, MethodBin)
    end.

find_method_spec_(Module, Specs, Method) ->
    ?debug("find_method_spec_(~p, ~p, ~p)~n", [Module, Specs, Method]),
    Yang = <<Module/binary, ".yang">>,
    case lists:keyfind(Yang, 2, Specs) of
	{_CfgName, _Y, Protocol} = _Found ->
	    ?debug("found spec = ~p~n", [_Found]),
	    find_method_rpcs_(Yang, Module, Method, Protocol);
	false ->
	    error
    end.

find_method_rpcs_(Yang, Module, Method, Protocol) ->
    case exodm_db_yang:rpcs(Yang) of
	[] ->
	    io:fwrite("No RPCs for ~p~n", [Yang]),
	    error;
	[{_, Spec}] ->
	    io:fwrite("Found RPCs of ~p~n"
		      "Methods = ~p~n", [Yang, catch [M || {M,_} <- Spec]]),
	    Key = binary_to_list(<<Module/binary,":",Method/binary>>),
	    case lists:keyfind(Key, 1, Spec) of
		{_, RPC} ->
		    io:fwrite("Method found: Mod = ~p;~n  RPC = ~P~n",
			      [Module,RPC,5]),
		    {ok, Yang, Module, Method, Protocol, RPC};
		false ->
		    case lists:keyfind(binary_to_list(Method),1,Spec) of
			{_,RPC} -> {ok, Module, Method, Protocol, RPC};
			false   -> error
		    end
	    end
    end.


get_default_module(AID, DID) ->
    case exodm_db_device:lookup_attr(AID, DID, yang) of
	[] ->
	    error;
	[{_, Yang}] ->
	    filename:basename(Yang, <<".yang">>)
    end.


validate_request(Method, Module, {struct, InputArgs},
		 {_Descr, {request,{struct,L}}, _Reply}) ->
    ?debug("validate_request(~p, ~p)~n", [Method,InputArgs]),
    case lists:keyfind("params", 1, L) of
	{_, {struct, Params}} ->
	    ?debug("Params = ~p~n", [Params]),
	    VerifyRes = do_validate(Method, Module, InputArgs, Params),
	    ?debug("VerifyRes = ~p~n", [VerifyRes]),
	    VerifyRes;
	_Other ->
	    ?debug("couldn't find 'params': ~p~n", [_Other]),
	    error
    end.

do_validate(Method, Module, Args, Spec) ->
    try  Res = {verified, convert_req(Module, Method, Args, Spec)},
	 ?debug("converted: Res = ~p~n", [Res]),
	 Res
    catch
	throw:Result ->
	    {not_verified, Result}
    end.

incr_id() ->
    try ets:update_counter(?TAB, rpc_id, {2,1,9999,1})
    catch
	error:_ ->
	    ets:insert(?TAB, {rpc_id, 1}),
	    1
    end.

queue_message(Db, AID, Tab, Env, {Type, Attrs, _} = Msg) when Type==request;
							  Type==notify;
							  Type==reply ->
    {_, DeviceID} = lists:keyfind(device_id, 1, Attrs ++ Env),
    Q = exodm_db_device:enc_ext_key(AID, DeviceID),
    Ret = case kvdb:push(Db, Tab, Q,
			 _Obj = {<<>>, Env, Msg}) of
	      {ok, AbsKey} ->
		  {ok, Q, AbsKey};
	      Error ->
		  Error
	  end,
    ?debug("queue_request(~p, ~p, ~p) ->~n"
	   "  ~p~n", [Tab, Env, Msg, Ret]),
    attempt_dispatch(Ret, Db, Tab, Q),
    Ret.

attempt_dispatch({ok, _, _} = _Ret, Db, Tab, Q) ->
    ?debug("attempt_dispatch(~p, ~p, ~p)~n", [_Ret, Tab, Q]),
    case exodm_rpc_dispatcher:attempt_dispatch(Db, Tab, Q) of
	{ok, Pid} ->
	    ?debug("dispatcher Pid = ~p~n", [Pid]),
	    MRef = erlang:monitor(process, Pid),
	    receive
		{Pid, exodm_rpc_dispatcher, done} ->
		    ?debug("dispatcher is done.~n", []),
		    erlang:demonitor(MRef, [flush]),
		    ok;
		{'DOWN', MRef, _, _, _Reason} ->
		    ?debug("dispatcher DOWN: ~p.~n", [_Reason]),
		    ok
	    end;
	pending ->
	    ok
    end;
attempt_dispatch(_, _, _, _) ->
    ok.



%% %% Do we queue this on IP or URL?
%% queue_reply(Reply, Attrs, {call, _Mod, Method, _Args} = _OriginalRequest) ->
%%     %% {_, IP} = lists:keyfind(ip, 1, Attrs),
%%     {_, DeviceID} = lists:keyfind(device_id, 1, Attrs),
%%     kvdb:push(rpc_queues, ck3_from_device, DeviceID, {reply, Method, Attrs, Reply}).


%% If we pushed on URL, we must also pop on URL
%% pop_and_deliver_reply(URL) ->
%%     case kvdb:pop(rpc_queues, ck3_from_device, URL) of
%% 	{ok, {Method, Attrs, Reply}} ->
%% 	    encode_and_deliver(Method, Attrs, Reply);
%% 	done ->
%% 	    done
%%     end.

%% encode_and_deliver({reply, Attrs, Reply} = R) ->
%%     Method = get_method(R),
%%     case lookup({Method, reply}) of
%% 	{ok, {_, _RAttrs, Spec}} ->
%% 	    JSON = {struct, to_json(Spec, Attrs, Reply)},
%% 	    post_json(Attrs, JSON);
%% 	Error ->
%% 	    Error
%%     end.

error_response({Error, Data}) ->
    {Code, Str} =
	case lists:keyfind(
	       Error, 1, [{invalid_params, -32602, "invalid params"},
			  {method_not_found, -32601,
			   "method not found"}]) of
	    {_, C, S} -> {C, S};
	    false     -> {-32603, "internal error"}
	end,
    {error, {struct, [{"code", Code},
		      {"message", Str},
		      {"data", lists:flatten(
				 io_lib:fwrite("~p", [Data]))}]}}.

convert_error(undef, Data) ->
    {method_not_found, Data};
convert_error(function_clause, Data) ->
    {invalid_params, Data};
convert_error(Other, Data) ->
    {Other, Data}.



%% copied from yaws_rpc (except that it doesn't call json2:encode/1 - let
%% yaws_rpc do that).
json_error(ErrCode) ->
    json_error(ErrCode, null).
json_error(ErrCode, Id) ->
    {struct, [{"jsonrpc", "2.0"},
	      {"id", Id},
	      {"error", {struct,
			 [{"code", ErrCode},
			  {"message", json_error_message(ErrCode)}]}}]}.

json_error_message(-32700) -> "parse error";
json_error_message(-32600) -> "invalid request";
json_error_message(-32601) -> "method not found";
json_error_message(-32602) -> "invalid params";
json_error_message(-32603) -> "internal error";
json_error_message(Code) when Code >= -32099, Code =< -32000 -> "server error";
json_error_message(_) -> "json error".


success_response(Result, Env, {request, Attrs, _},
		 {_, _, {reply, {struct, Elems}}}) ->
    ?debug("success_response(~p, Env = ~p, Attrs = ~p, Elems = ~p)~n",
	   [Result, Env, Attrs, Elems]),
    {_, {struct, ResultSpec}} = lists:keyfind("result", 1, Elems),
    ?debug("ResultSpec = ~p~n", [ResultSpec]),
    {_, TID} = lists:keyfind(transaction_id, 1, Attrs),
    {struct, to_json_(ResultSpec, Attrs ++ Env,
		      [{'transaction-id', TID}|Result], [])}.


accept_response(Attrs, {_, _, {reply, {struct, Elems}}} = Spec) ->
    ?debug("success_response(~p, ~p)~n", [Attrs, Spec]),
    case lists:keyfind("result", 1, Elems) of
	{_, void} ->
	    "\"ok\"";
	{_, {struct, ResultSpec}} ->
	    {_, TID} = lists:keyfind(transaction_id, 1, Attrs),
	    {struct, to_json_(ResultSpec, Attrs,
			      [{'transaction-id', TID},
			       {'rpc-status', <<"accepted">>},
			       {final, false}], [])}
    end.
%% accept_response(Attrs, {request, Attrs, {call, _M, ShortMeth, _}}) ->
%%     {_, TID} = lists:keyfind(transaction_id, 1, Attrs),
%%     {_, Pfx} = lists:keyfind(prefix, 1, Attrs),
%%     Method = join_method(Pfx, ShortMeth),
%%     case lookup({Method, reply}) of
%% 	{ok, {_, _, Spec}} ->
%% 	    {struct, to_json_(Spec, Attrs, [{'transaction-id', TID},
%% 					    {'rpc-status', <<"accepted">>},
%% 					    {final, false}], [])};
%% 	_ ->
%% 	    error({unknown_response, Method})
%%     end.

get_method({request, _, {call, _Mod, Method, _}}) ->
    Method;
get_method({reply, As, _Msg}) ->
    {_, Method} = lists:keyfind(method, 1, As),
    Method;
get_method({notify, As, _Msg}) ->
    {_, Method} = lists:keyfind(method, 1, As),
    Method.

post_json(Env, JSON) ->
    {_, DID} = lists:keyfind(device_id, 1, Env),
    {_, AID} = lists:keyfind(aid, 1, Env),
    case exodm_db_device:lookup_group_notifications(AID, DID) of
	[] ->
	    ok;
	[_|_] = URLs ->
	    Body = json2:encode(JSON),
	    Hdrs = [
		    {"content-length", integer_to_list(iolist_size(Body))},
		    {"host", "localhost"}
		   ],
	    [lhttpc:request(
	       binary_to_list(URL), "POST", Hdrs, Body, 1000) || URL <- URLs],
	    ok
    end.

msg_to_json(request, Method, Attrs, Msg) ->
    case lookup({Method, request}) of
	{ok, {_, _RAttrs, Spec}} ->
	    io:format("request spec: ~p\n", [Spec]),
	    {struct,
	     [{"jsonrpc", "2.0"},
	      {"id", integer_to_list(incr_id())},
	      {"method", atom_to_list(Method)},
	      {"params", {struct, to_json_(Spec, Attrs, Msg, [])}}
	     ]};
	Error ->
	    Error
    end;
msg_to_json(notify, Method0, Attrs, Msg) ->
    Method = case lists:keyfind(prefix, 1, Attrs) of
		 {_, ""} -> Method0;
		 {_, Pfx} -> list_to_atom(Pfx ++ ":" ++ atom_to_list(Method0))
	     end,
    case lookup({Method, notify}) of
	{ok, {_, _RAttrs, Spec}} ->
	    {struct,
	     [{"jsonrpc", "2.0"},
	      {"method", atom_to_list(Method)},
	      {"params", {struct, to_json_(Spec, Attrs, Msg, [])}}
	     ]};
	Error ->
	    Error
    end.

to_json({struct, L}, Attrs, Reply) ->
    {struct, to_json_(L, Attrs, Reply, [])};
to_json({array, L}, Attrs, Reply) ->
    {array, to_json_(L, Attrs, Reply, [])};
to_json(L, Attrs, Reply) when is_list(L) ->
    {array, to_json_(L, Attrs, Reply, [])}.

to_json_([{"rpc-status-string", [], _, _}|T], Attrs, Reply, Hist) ->
    case lists:keyfind('rpc-status-string', 1, Reply) of
	{_, S} when is_list(S); is_binary(S) ->
	    [{'rpc-status-string', S}
	     | to_json_(T, Attrs, Reply, Hist)];
	false ->
	    case lists:keyfind('rpc-status', 1, Reply) of
		{_, St} ->
		    D = case lists:keyfind("rpc-status", 1, Hist ++ T) of
			    {_, [], _, {type,_,<<"enumeration">>,En}} ->
				enum_descr(St, En)
			end,
		    [{'rpc-status-string', D} | to_json_(T, Attrs, Reply,
							 Hist)]
	    end
    end;
to_json_([{K, [], _, Type} = H|T], Attrs, Reply, Hist) ->
    Ka = list_to_atom(K),
    case lists:keyfind(Ka, 1, Reply) of
	{_, X} ->
	    X1 = yang_json:to_json_type(X, Type),
	    [{K, X1}
	     | to_json_(T, Attrs, Reply, keep_history(H,Hist))];
	false ->
	    case lists:keyfind(Ka, 1, Attrs) of
		{_, X} ->
		    X1 = yang_json:to_json_type(X, Type),
		    [{K, X1}
		     | to_json_(T, Attrs, Reply, keep_history(H, Hist))];
		_ ->
		    error({cannot_convert_to_json, Reply})
	    end
    end;
to_json_([{K, {array,[Ch]}, _, _}|T], Attrs, Reply, Hist) ->
    Ka = list_to_atom(K),
    case lists:keyfind(Ka, 1, Reply) of
	{_, {array, L}} when is_list(L) ->
	    [{Ka, {array, to_json_array_(Ch, Attrs, L, Hist)}}
	     | to_json_(T, Attrs, Reply, Hist)];
	false ->
	    error({cannot_convert_to_json, Reply})
    end;
to_json_([{K, {struct, Ch}, _, _}|T], Attrs, Reply, Hist) ->
    Ka = list_to_atom(K),
    case lists:keyfind(Ka, 1, Reply) of
	{_, {struct, L}} when is_list(L) ->
	    [{K, {struct, to_json_(Ch, Attrs, L, Hist)}}
	     | to_json_(T, Attrs, Reply, Hist)];
	false ->
	    case lists:keyfind(Ka, 1, Attrs) of
		{_, L} when is_list(L) ->
		    [{K, {struct, to_json_(L, Attrs, Ch, Hist)}}
		     | to_json_(T, Attrs, Reply, Hist)];
		_ ->
		    error({cannot_convert_to_json, Reply})
	    end
    end;
to_json_([], _, _, _) ->
    [].

to_json_array_({[],[],_,Type}, _Attrs, Reply, _Hist) ->
    [yang_json:to_json_type(X, Type) || X <- Reply];
to_json_array_({struct, L}, Attrs, Reply, Hist) ->
    [{struct, to_json_(L, Attrs, R, Hist)} || {struct, R} <- Reply].

keep_history({"rpc-status",_,_,_} = X, H) ->
    [X|H];
keep_history(_, H) ->
    H.


enum_descr(St, En) ->
    case [lists:keyfind(description, 1, I) || {enum,_,E,I} <- En,
					      E == St] of
	[{_, _, D,_}] ->
	    D;
	_ ->
	    error({cannot_find_description, [St, En]})
    end.

convert_req(Mod, Method, Args, Spec) ->
    %% Conv = convert_req_(Args, Spec),
    Conv = convert_req_(Spec, Args),
    TID = proplists:get_value('transaction-id', Conv, 1),
    {request, [{transaction_id, TID}],
     {call, to_atom(Mod), to_atom(Method), Conv}}.


to_atom(L) when is_list(L) ->
    list_to_atom(L);
to_atom(B) when is_binary(B) ->
    binary_to_atom(B, latin1).

join_method("", M) ->
    M;
join_method(Pfx, M) ->
    list_to_atom(Pfx ++ ":" ++ atom_to_list(M)).


split_method(Mod, M) ->
    case re:split(atom_to_list(M), ":", [{return,list}]) of
	[_] ->
	    {Mod, M};
	[P,Ms] ->
	    {P, list_to_atom(Ms)}
    end.


convert_req_([{K, Ch, _Descr, Type}|Spec1], Req) ->
    case lists:keytake(K, 1, Req) of
	{value, {_, V}, Req1} ->
	    case Type of
		{type, _, <<"anyxml">>, _} ->
		    %% Do not convert; keep original (decoded) JSON
		    [] = Ch,  % assertion
		    [{list_to_atom(K), V} | convert_req_(Spec1, Req1)];
		_ ->
		    convert_req_(K, V, Ch, Type, Spec1, Req1)
	    end;
	false ->
	    AlsoMissing = [K1 || {K1,_,_,_} <- Spec1,
				 not(lists:keymember(K1, 1, Spec1))],
	    throw({invalid_params, {required, [K|AlsoMissing]}})
    end;
convert_req_([], []) ->
    [];
convert_req_([], [_|_] = Unknown) ->
    throw({invalid_params, {unknown_params, [element(1, U) || U <- Unknown]}}).

convert_req_(K, V, Ch, Type, Spec1, Req1) ->
    case {V, Ch} of
	{{array, Sub}, {array,[SubSpec]}} ->
	    [{list_to_atom(K), {array, convert_array_(Sub, SubSpec)}}
	     | convert_req_(Spec1, Req1)];
	{{St, Sub}, {St, SubSpec}} when St==struct; St==array ->
	    [{list_to_atom(K), convert_req_(SubSpec, Sub)}
	     | convert_req_(Spec1, Req1)];
	{_, []} ->
	    case yang:check_type(V, Type) of
		{true, Val} ->
		    [{list_to_atom(K), Val} | convert_req_(Spec1, Req1)];
		false ->
		    ?debug("Wrong type: ~p (~p)~n", [Type, Spec1]),
		    throw({invalid_params, {wrong_type, [K, V, Type]}})
	    end;
	{_, Expected} ->
	    throw({invalid_params, {mismatch, [K, V, Expected]}})
    end.

%% convert_req_([{K, V}|T], Spec) ->
%%     case lists:keytake(K, 1, Spec) of
%% 	{value, {_, [], _Descr, {type, _, <<"anyxml">>, _}},  Spec1} ->
%% 	    %% Do not convert; keep original (decoded) JSON
%% 	    [{list_to_atom(K), V} | convert_req_(T, Spec1)];
%% 	{value, {_, Ch, _Descr, Type}, Spec1} ->
%% 	    case {V, Ch} of
%% 		{{array,Sub}, {array,[SubSpec]}} ->
%% 		    [{list_to_atom(K), {array, convert_array_(Sub, SubSpec)}}
%% 		     | convert_req_(T, Spec1)];
%% 		{{St,Sub}, {St, SubSpec}} when St==struct; St==array ->
%% 		    [{list_to_atom(K), convert_req_(Sub, SubSpec)}
%% 		     | convert_req_(T, Spec1)];
%% 		{_, []} ->
%% 		    case yang:check_type(V, Type) of
%% 			{true, Val} ->
%% 			    [{list_to_atom(K), Val}|convert_req_(T, Spec1)];
%% 			false ->
%% 			    ?debug("Wrong type: ~p (~p)~n", [{K,V}, Type]),
%% 			    throw({invalid_params, {wrong_type, [K,V,Type]}})
%% 		    end;
%% 		{V, Expected} ->
%% 		    throw({invalid_params, {mismatch, [K, V, Expected]}})
%% 	    end;
%% 	false ->
%% 	    throw({invalid_params, {unknown_param, [K]}})
%%     end;
%% convert_req_([], []) ->
%%     [];
%% convert_req_([], [_|_] = Required) ->
%%     throw({invalid_params, {required, [element(1, R) || R <- Required]}}).


%% convert_req_([{K, V}|T], Spec) ->
%%     case lists:keyfind(K, 1, Spec) of
%% 	{_, Ch, _Descr, Type} ->
%% 	    case {V, Ch} of
%% 		{{array,Sub}, {array,[SubSpec]}} ->
%% 		    [{list_to_atom(K), {array, convert_array_(Sub, SubSpec)}}
%% 		     | convert_req_(T, Spec)];
%% 		{{St,Sub}, {St, SubSpec}} when St==struct; St==array ->
%% 		    [{list_to_atom(K), convert_req_(Sub, SubSpec)}
%% 		     | convert_req_(T, Spec)];
%% 		{_, []} ->
%% 		    case yang:check_type(V, Type) of
%% 			{true, Val} ->
%% 			    [{list_to_atom(K), Val}|convert_req_(T, Spec)];
%% 			false ->
%% 			    ?debug("Wrong type: ~p (~p)~n", [{K,V}, Type]),
%% 			    throw({wrong_type, [K,V,Type]})
%% 		    end;
%% 		{V, Expected} ->
%% 		    throw({mismatch, [K, V, Expected]})
%% 	    end;
%% 	false ->
%% 	    throw({unknown_param, [K]})
%%     end;
%% convert_req_([], _) ->
%%     [].


convert_array_([{struct, _}|_] = L, {struct, Spec}) ->
    lists:map(fun({struct, X}) ->
		      {struct, convert_req_(Spec, X)}
		      %% {struct, convert_req_(X, Spec)}
	      end, L);
convert_array_(L, Type) ->
    lists:map(fun(X) ->
		      case yang:check_type(X, Type) of
			  {true, V} -> V;
			  false -> throw(illegal)
		      end
	      end, L).

start_link() ->
    create_ets(),
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

create_ets() ->
    create_ets(?TAB).

create_ets(T) ->
    case ets:info(T, name) of
	undefined ->
	    ets:new(T, [named_table, public, ordered_set]);
	_ ->
	    true
    end.

store(T, Obj) ->
    ets:insert(T, Obj).

%% mimick kvdb:get/3
lookup(K) ->
    try lookup_ret(ets:lookup(?TAB, K))
    catch
	error:badarg ->
	    %% assume this can happen while the specs are being reloaded
	    receive after 100 -> ok end,
	    lookup_ret(ets:lookup(?TAB, K))
    end.

lookup_ret([Obj]) -> {ok, Obj};
lookup_ret([])    -> {error, not_found}.


init([]) ->
    {ok, #st{}}.


%% handle_call(load_specs, From, S) ->
%%     gen:reply(From, ok),
%%     do_load_specs(?TAB),
%%     {noreply, S};
%% handle_call(reload_specs, From, S) ->
%%     create_ets(?TMP_TAB),
%%     %% The following will result in an error report, since the supervisor will
%%     %% receive an unexpected message
%%     ets:give_away(?TMP_TAB, whereis(exodm_rpc_sup), []),
%%     gen:reply(From, ok),
%%     do_load_specs(?TMP_TAB),
%%     %% There will be a tiny time gap here when the table doesn't exist.
%%     ets:delete(?TAB),
%%     ets:rename(?TMP_TAB, ?TAB),
%%     {noreply, S};
handle_call(_Req, _From, S) ->
    {noreply, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.