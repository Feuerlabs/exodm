-module(exodm_rpc_handler).

-compile(export_all).
-export([handler_session/1]).
-export([notification/5, notification/6,    % don't use notification/6!
	 queue_message/5]).
-export([int_json_rpc/1]).

-export([device_sessions/1, device_sessions/2]).
-export([add_device_session/2, add_device_session/3]).
-export([rm_device_session/2, rm_device_session/3]).
-export([std_specs/0]).
-export([request_timeout/6]).

-include_lib("lager/include/log.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include_lib("lhttpc/include/lhttpc.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% -type ext_id() :: binary().   %% External representation of AID+DID
%% -type aid() :: binary().      %% Account ID
%% -type did() :: binary().      %% Device ID
%% -type protocol() :: binary(). %% <<"exodm_bert" | "exodm" | "exodm_ck3">>

%% @spec add_device_session(aid(), did(), protocol()) -> true.
%% @doc Register an active device session
%%
%% This enables the rpc handler to locate the device session process.
%% @end
add_device_session(AID, DID, Protocol) ->
    ExtID = exodm_db_device:enc_ext_key(AID, DID),
    add_device_session(ExtID, Protocol).

add_device_session(ExtID, Protocol) ->
    gproc:reg({p,l,{exodm_rpc, active_device, ExtID, Protocol}}).

%% @spec rm_device_session(aid(), did(), protocol()) -> true.
%% @doc Removes the session registration
%%
%% This does not need to be done if the process will terminate anyway.
%% @end
rm_device_session(AID, DID, Protocol) ->
    rm_device_session(exodm_db_device:enc_ext_key(AID, DID), Protocol).

rm_device_session(ExtID, Protocol) ->
    catch gproc:unreg({p,l,{exodm_rpc, active_device, ExtID, Protocol}}),
    true.

%% @spec device_sessions(aid(), did()) -> [{pid(), protocol()}].
%% @doc List active sessions for a given device
%% @end
device_sessions(AID, DID) ->
    ExtID = exodm_db_device:enc_ext_key(AID, DID),
    device_sessions(ExtID).

device_sessions(ExtID) ->
    Res = gproc:select(p, [{ {{p,l,{exodm_rpc, active_device, ExtID, '$2'}},
			      '$1', '_'},
			     [], [{{'$1', '$2'}}] }]),
    ?debug("device_sessions(~p) -> ~p~n", [ExtID, Res]),
    Res.

%% @doc Handle a JSON-RPC request; once go-ahead is given from load control.
handler_session(Arg) ->
    jobs:run(
      exodm_rpc_from_web,
      fun() ->
	      try
		  ?debug("handler_session(~p)~n", [Arg]),
		  Peer = peername(Arg#arg.clisock),
		  {ok,{IP,_}} = Peer,
		  Arg2=Arg#arg{state = [{ip, IP}, {client,json_rpc}]},
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

int_json_rpc(Req) ->
    try kvdb_conf:in_transaction(
	  fun(Db) ->
		  case web_rpc_(Db, [{client, erlang_json}], Req) of
		      {true, {response, Resp}} ->
			  {true, Resp};
		      Other -> Other
		  end
	  end)
    catch
	error:E ->
	    ?error("*** ERROR! ~p:int_json_rpc(~p)~n"
		   "  E = ~p~n"
		   "  Trace = ~p~n",
		   [?MODULE, Req, E, erlang:get_stacktrace()]),
	    {false, error_response({internal_error,
				    lists:flatten(io_lib:format("~p", [E]))})};
	throw:{error_response, Err, Data} ->
	    {false, error_response({Err, Data})}
    end.

web_rpc(St, Req, Session) ->
    try kvdb_conf:in_transaction(
	  fun(Db) ->
		  case web_rpc_(Db, St, Req) of
		      {true, Response} -> {true, 0, Session, Response};
		      Other -> Other
		  end
	  end)
    catch
	error:E ->
	    ?error("*** ERROR! ~p:web_rpc(~p, ~p, ~p)~n"
		   "  E = ~p~n"
		   "  Trace = ~p~n",
		   [?MODULE, St, Req, Session, E, erlang:get_stacktrace()]),
	    {false, error_response({internal_error,
				    lists:flatten(io_lib:format("~p", [E]))})};
	throw:{error_response, Err, Data} ->
	    {false, error_response({Err, Data})}
    end.

web_rpc_(Db, InitEnv, {call, Method, Request} = RPC0) ->
    ?debug("web_rpc: Method = ~p; Request = ~p~n", [Method, Request]),
    AID = exodm_db_session:get_aid(),
    UID = exodm_db_session:get_user(),
    Env0 = [{aid, AID},
	    {user, UID}|InitEnv],
    case json_get_device_id(Request) of
	{ok, DID} ->
	    %% We assume if it doesn't have a device-id argument, it can't be
	    %% a device-specific RPC. However, the presence of device-id doesn't
	    %% guarantee that it is, so we check for a device-specific method
	    %% first; if that fails, we check system rpcs.
	    check_if_device_exists(AID, DID),
	    ?debug("found device-id: ~p~n", [DID]),
	    case find_method_spec(Method, AID, DID) of
		{ok, Yang, Module, ShortMeth, Protocol, URL, Spec} ->
		    ?info("Method spec (~p): ~p~n"
			   "Module = ~p~n", [Method, Spec, Module]),
		    Env1 =
			[{yang, Yang},
			 {'device-id', DID}] ++
			[{'notification-url', URL} || URL =/= <<>>] ++
			[{protocol, Protocol}|Env0],
		    case validate_request(
			   ShortMeth, Module, Request, Spec) of
			{ok, Attrs, Meta} ->
			    RPC = {call, Module, ShortMeth, Attrs},
			    ?debug("request verified: ~p~n", [RPC]),
			    Env2 = get_tid(Attrs, [{yang_meta, Meta}|Env1], AID),
			    queue_message(
			      Db, AID, to_device, Env2, RPC),
			    Response = accept_response(Attrs ++ Env2, Spec),
			    {true, {response, Response}};
			{error, Reason} ->
			    ?debug("request NOT verified: ~p~n", [Reason]),
			    Response = error_response(Reason),
			    {false, Response}
		    end;
		error ->
		    web_rpc_system_(Db, AID, Env0, RPC0)
	    end;
	error ->
	    web_rpc_system_(Db, AID, Env0, RPC0)
    end.

web_rpc_system_(Db, AID, Env0, {call, Method, Request}) ->
    %% Check if this is a general RPC, supported by one of the system
    %% specs. Otherwise, it's an error.
    ?debug("no device-id~n", []),
    case is_exodm_method(Method, AID) of
	{ok, Yang, Module, ShortMeth, Protocol, URL, Spec} ->
	    ?debug("ExoDM method: ~p; Module = ~p; ShortM = ~p~n",
		   [Method, Module, ShortMeth]),
	    Env1 = [{yang, Yang},
		    {protocol, Protocol}]
		++ [{'notification-url', URL} || URL =/= <<>>] ++ Env0,
	    case validate_request(
		   ShortMeth, Module, Request, Spec) of
		{ok, Attrs, Meta} ->
		    Env2 = get_tid(Attrs, [{yang_meta, Meta}|Env1], AID),
		    RPC1 = {call, Module, ShortMeth, Attrs},
		    ?debug("request verified: ~p~n", [RPC1]),
		    handle_exodm_rpc(Protocol, Env2, RPC1, Spec);
		{error, Reason} ->
		    ?debug("request NOT verified: ~p~n", [Reason]),
		    Response = error_response(Reason),
		    {false, Response}
	    end;
	error ->
	    ?debug("is_exodm_method(~p, ~p) -> error~n", [Method,AID]),
	    {false, error_response({method_not_found, Method})}
    end.


check_if_device_exists(AID, DID) ->
    case exodm_db_device:exist(AID, DID) of
	true ->
	    ok;
	false ->
	    throw({error_response, 'device-unknown',
		   ["Device ", DID, "doesn't exist"]})
    end.

handle_exodm_rpc(Protocol, Env, RPC, Spec) ->
    Mod = exodm_rpc_protocol:module(Protocol),
    ?debug("handle_exodm_rpc(~p, ~p, ~p); Mod = ~p~n",
	   [Protocol, Env, RPC, Mod]),
    try Mod:json_rpc(RPC, Env) of
	{ok, Result} = _OK when is_list(Result) ->
	    ?debug("~p:json_rpc(...) -> ~p~n", [?MODULE, _OK]),
	    Resp = success_response(Result, Env, RPC, Spec),
	    ?debug("Resp = ~p~n", [Resp]),
	    {true, {response, Resp}};
	{error, Err} = _Error ->
	    ?debug("~p:json_rpc(...) -> ~p~n", [?MODULE, _Error]),
	    {false, error_response(convert_error(Err, RPC))};
	Other ->
	    ?error("Erroneous callback return:~n"
		   "~p:json_rpc(~p, ~p) -> ~p~n", [Mod,RPC,Env,Other]),
	    error({bad_callback_return, Other})
    catch error:E ->
	    ?debug("~p:json_rpc(...) -> *ERROR*:~p~n",
		   [?MODULE, {E, erlang:get_stacktrace()}]),
	    {false, error_response(convert_error(E, RPC))}
    end.

notification(Method, Elems, Env, _Req, AID, DID) ->
    %% Should be removed.
    notification(Method, Elems, Env, AID, DID).

notification(Method, Elems, Env, AID, DID) ->
    ?debug("notification(~p, ~p, ~p, ~p, ~p)~n",
	   [Method, Elems, Env, AID, DID]),
    {_, Yang} = lists:keyfind(yang, 1, Env),
    stop_timer(Env),
    YangSpecs = exodm_db_device:yang_modules(AID, DID),
    URLEnv = case lists:keyfind(Yang, 2, YangSpecs) of
		 {_, _, URL} when URL =/= <<>> ->
		     [{'notification-url', URL}];
		 _ ->
		     []
	     end,
    [Module|_] = re:split(filename:basename(Yang, ".yang"), "@",
			  [{return,binary}]),
    FullMethod = case re:split(to_binary(Method), ":", [{return,binary}]) of
		     [Meth] ->
			 <<Module/binary, ":", Meth/binary>>;
		     [A,B] ->
			 <<A/binary, ":", B/binary>>
		 end,
    case exodm_db_yang:find_rpc(Yang, FullMethod) of
	{error, not_found} ->
	    error(method_not_found);
	{ok, {_, _, {notification,_,_,SubSpec}}} ->
	    ?debug("SubSpec = ~p~n", [lists:sublist(SubSpec,1,2)]),
	    Params = data_to_json(SubSpec, Env, Elems),
	    JSON = {struct, [{"jsonrpc", "2.0"},
			     {"method", FullMethod},
			     {"params", {struct, Params}}]},
	    ?debug("JSON = ~p~n", [JSON]),
	    post_json(URLEnv ++ Env, JSON);
	{ok, {_, _, {rpc, _, _, SubSpec} = RPC}} ->
	    {input, _, _, InputSpec} = lists:keyfind(input, 1, SubSpec),
	    ?debug("Northbound RPC = ~p~n", [RPC]),
	    Params = data_to_json(InputSpec, Env, Elems),
	    JSON = {struct, [{"jsonrpc", "2.0"},
			     {"method", FullMethod},
			     {"id", make_id(Env, AID)},
			     {"params", {struct, Params}}]},
	    ?debug("JSON = ~p~n", [JSON]),
	    post_json(URLEnv ++ Env, JSON)
    end.

stop_timer(Env) ->
    case lists:keyfind('$timer_id', 1, Env) of
	{_, TID} ->
	    ?debug("Deleting timer ~p~n", [TID]),
	    kvdb_cron:delete(kvdb_conf, rpc_timers, TID);
	false ->
	    ok
    end.

qualified_method(M, Mod) ->
    case binary:split(M, <<":">>) of
	[_, _] ->
	    M;
	[_] ->
	    <<Mod/binary, ":", M/binary>>
    end.

make_id(Env, AID) ->
    case lists:keyfind(id, 1, Env) of
	false ->
	    <<I:32>> = exodm_db_account:incr_request_id(AID),
	    integer_to_list(I);
	{_, ID} ->
	    id_to_list(ID)
    end.

id_to_list(ID) when is_integer(ID) ->
    integer_to_list(ID);
id_to_list(ID) when is_list(ID) ->
    _ = list_to_integer(ID),  % assertion
    ID;
id_to_list(ID) when is_binary(ID) ->
    integer_to_list(
      list_to_integer(binary_to_list(ID))).

get_tid(Attrs, Env, AID) ->
    case keyfind(<<"transaction-id">>, Attrs) of
	false ->
	    <<I:32>> = exodm_db_account:incr_transaction_id(AID),
	    [{'transaction-id', integer_to_list(I)}|Env];
	Found ->
	    [{'transaction-id', id_to_list(element(2,Found))}|Env]
    end.

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

to_binary(A) when is_atom(A) ->
    atom_to_binary(A, latin1);
to_binary(I) when is_integer(I) ->
    list_to_binary(integer_to_list(I));
to_binary(L) when is_list(L) ->
    list_to_binary(L);
to_binary(B) when is_binary(B) ->
    B.



is_exodm_method(Method, AID) ->
    case binary:split(to_binary(Method), <<":">>) of
	[MethodBin] ->
	    Mod = <<"exodm">>,
	    ?debug("Mod = ~p; MethodBin = ~p~n", [Mod, MethodBin]),
	    find_method_spec_(Mod, [<<"exodm">>], MethodBin, <<"exodm">>);
	[Mod, MethodBin] ->
	    ?debug("Mod = ~p; MethodBin = ~p~n", [Mod, MethodBin]),
	    YangSpecs = annotate_specs(
			  std_specs() ++ exodm_db_account:system_specs(AID)),
	    find_method_spec_(Mod, YangSpecs, MethodBin, <<"exodm">>)
    end.

std_specs() ->
    %% FIXME: "exodm" should be retired and replaced by "exosense"
    %% (by name, mainly; the two specs shall be merged into one)
    [<<"exodm">>, <<"exosense">>].

json_get_device_id({struct, L}) ->
    case lists:keyfind("device-id", 1, L) of
	{_, ID} ->
	    {ok, list_to_binary(ID)};
	false ->
	    error
    end.

annotate_specs(Specs) ->
    [{<<>>, <<S/binary, ".yang">>} || S <- Specs].

find_method_spec(Method, AID, DevID) ->
    ?debug("find_method_spec(~p, ~p)~n", [Method, DevID]),
    DID = exodm_db:encode_id(DevID),
    case exodm_db_device:yang_modules(AID, DID) of
	[] ->
	    ?debug("no yang specs mapped to device (~p/~p)~n", [AID, DID]),
	    error;
	YangSpecs ->
	    ?debug("yang specs mapped to device (~p/~p): ~p~n",
		   [AID, DID, YangSpecs]),
	    Protocol = exodm_db_device:protocol(AID, DID),
	    case binary:split(to_binary(Method), <<":">>) of
		[MethodBin] ->
		    Mod = get_default_module(AID, DID),
		    find_method_spec_(Mod, YangSpecs, MethodBin, Protocol);
		[Mod, MethodBin] ->
		    find_method_spec_(Mod, YangSpecs, MethodBin, Protocol)
	    end
    end.

find_method_spec_(Module, Specs, Method, Protocol) ->
    ?debug("find_method_spec_(~p, ~p, ~p)~n", [Module, Specs, Method]),
    Yang = <<Module/binary, ".yang">>,
    case lists:keyfind(Yang, 2, Specs) of
	{_CfgName, _Y} = _Found ->
	    ?debug("found spec in = ~p~n", [_CfgName]),
	    find_method_(Yang, Module, Method, Protocol, <<>>);
	{_CfgName, _Y, URL} = _Found ->
	    ?debug("found spec in = ~p~n", [_CfgName]),
	    find_method_(Yang, Module, Method, Protocol, URL);
	false ->
	    error
    end.

find_method_(Yang, Module, Method, Protocol, URL) ->
    case exodm_db_yang:find_rpc(Yang, <<Module/binary, ":", Method/binary>>) of
	{ok, {_, _, RPC}} ->
	    ?debug("Method found: Mod = ~p;~n  RPC = ~P~n",
		   [Module,RPC,5]),
	    {ok, Yang, Module, Method, Protocol, URL, RPC};
	{error, _} ->
	    error
    end.

get_default_module(AID, DID) ->
    case exodm_db_device:lookup_attr(AID, DID, yang) of
	[] ->
	    error;
	[{_, Yang}] ->
	    filename:basename(Yang, <<".yang">>)
    end.


validate_request(Method, _Module, {struct, InputArgs},
		 {rpc, _, Method, Spec}) ->
    case lists:keyfind(input, 1, Spec) of
	{input, _, _, Elems} ->
	    ?debug("validating~nInputArgs = ~p~nElems = ~p~n",
		   [InputArgs, Elems]),
	    yang_json:validate_rpc_request(Elems, InputArgs);
	false ->
	    {error, {invalid_params, no_input_statement}}
    end.

queue_message(Db, AID, Tab, Env, {call, _, _, Attrs} = Msg) ->
    queue_message_(Db, AID, Tab, Attrs, Env, Msg);
queue_message(Db, AID, Tab, Env, {Type,_Meth,Elems} = Msg)
  when Type==notify; Type==reverse_request ->
    queue_message_(Db, AID, Tab, Elems, Env, Msg).

queue_message_(Db, AID, Tab, Attrs, Env0, Msg) ->
    TimerID = make_ref(),
    Env = [{'$timer_id', TimerID}|Env0],
    AllAttrs = Attrs ++ Env,
    DeviceID = element(2, lists:keyfind('device-id', 1, AllAttrs)),
    Q = exodm_db_device:enc_ext_key(AID, DeviceID),
    Ret = case kvdb:push(Db, Tab, Q,
			 Obj = {<<>>, Env, Msg}) of
	      {ok, AbsKey} ->
		  maybe_start_timer(Msg, Db, Tab, TimerID, AbsKey, Obj),
		  {ok, Q, AbsKey};
	      Error ->
		  Error
	  end,
    ?debug("queue_request(~p, ~p, ~p) ->~n"
	   "  ~p~n", [Tab, Env, Msg, Ret]),
    attempt_dispatch(Ret, Db, Tab, Q),
    Ret.


maybe_start_timer({call,_,_,Args}, Db, Tab, TimerID, Key, Obj) ->
    case lists:keyfind('timeout', 1, Args) of
	{_, Timeout, _} ->
	    ?debug("Request timeout = ~p; starting timer~n", [Timeout]),
	    TimerQ = <<>>,
	    Res = kvdb_cron:add(Db, rpc_timers, TimerQ, Timeout, [{id, TimerID}],
				?MODULE, request_timeout,
				[TimerID, TimerQ,
				 kvdb:db_name(Db), Tab, Key, Obj]),
	    ?debug("Timer requested = ~p~n", [Res]),
	    Res;
	false ->
	    ok
    end;
maybe_start_timer(_, _, _, _, _, _) ->
    ok.


request_timeout(TimerID, TimerQ, Db, Tab, Key, Obj) ->
    ?debug("request_timeout(~p, ~p, ~p, ~p, ~p, ~p)~n",
	   [TimerID, TimerQ, Db, Tab, Key, Obj]),
    kvdb:delete(Db, Tab, Key),
    kvdb_cron:delete(Db, rpc_timers, TimerQ, TimerID),
    {_, Env, _} = Obj,
    case lists:keyfind(protocol, 1, Env) of
	{_, Protocol} ->
	    Mod = exodm_rpc_protocol:module(Protocol),
	    ?debug("Protocol = ~p; Mod = ~p~n", [Protocol, Mod]),
	    Mod:request_timeout(Obj);
	false ->
	    {error, no_protocol}
    end.


attempt_dispatch({ok, _, _} = _Ret, Db, Tab, Q) ->
    ?debug("attempt_dispatch(~p, ~p, ~p)~n", [_Ret, Tab, Q]),
    catch exodm_rpc_dispatcher:attempt_dispatch(Db, Tab, Q);
attempt_dispatch(_, _, _, _) ->
    ok.


error_response({Error, _Data} = E) ->
    ?debug("error_response(~p)~n", [E]),
    {Code, Str} =
	case lists:keyfind(
	       Error, 1, [{invalid_params, -32602, "invalid params"},
			  {method_not_found, -32601, "method not found"}]) of
	    {_, C, S} -> {C, S};
	    false     -> {-32603, "internal error"}
	end,
    {error, {struct, [{"code", Code},
		      {"message", Str},
		      {"data", pp_data(Error)}]}}.

pp_data(A) when is_atom(A) ->
    atom_to_binary(A, latin1);
pp_data(Data) when is_binary(Data) -> Data;
pp_data({wrong_type, [Key, Val, {type,_,Type,_}]}) ->
    lists:flatten(io_lib:fwrite("Wrong type: Attr=~s; Value=~p; Expected=~s",
				[Key, Val, Type]));
pp_data(Data) when is_list(Data) ->
    try iolist_to_binary(Data)
    catch
	error:_ ->
	    lists:flatten(io_lib:fwrite("~p", [Data]))
    end;
pp_data({Other, Data}) ->
    lists:flatten(io_lib:fwrite("Unknown error: ~p; ~p~n", [Other, Data])).


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


success_response(Result, Env, {call, _, _, _},
		 {rpc, _, _, Spec}) when is_list(Result) ->
    case lists:keyfind(output, 1, Spec) of
	{output, _, _, Elems} ->
	    ?debug("success_response(~p, Env = ~p, Elems = ~p)~n",
		   [Result, Env, Elems]),
	    JSON = data_to_json(Elems, Env, Result),
	    {struct, JSON}
    end.


accept_response(Attrs, {rpc, _, _, Spec}) ->
    ?debug("~p:accept_response(~p, ...)~n", [?MODULE, Attrs]),
    case lists:keyfind(output, 1, Spec) of
	{output, _, _, Elems} ->
	    %% {_, TID} = lists:keyfind('transaction-id', 1, Attrs),
	    JSON = data_to_json(
		     Elems, Attrs, [{'rpc-status', <<"accepted">>},
				    {final, false}]),
	    {struct, JSON};
	false ->
	    "\"ok\""
    end.

data_to_json(Elems, Env, Data) ->
    ?debug("data_to_json(~p, ~p, ~p)~n", [Elems, Env, Data]),
    case find_leaf(<<"rpc-status-string">>, Elems) of
	false ->
	    yang_json:data_to_json(Elems, Env, Data);
	_Leaf ->
	    case keyfind(<<"rpc-status-string">>, Data) of
		false ->
		    case keyfind(<<"rpc-status">>, Data) of
			false ->
			    yang_json:data_to_json(Elems, Env, Data);
			Status ->
			    case enum_descr(find_leaf(<<"rpc-status">>, Elems),
					    to_binary(element(2, Status))) of
				false ->
				    yang_json:data_to_json(Elems, Env, Data);
				Descr ->
				    yang_json:data_to_json(
				      Elems, Env,
				      [{<<"rpc-status-string">>, Descr}|Data])
			    end
		    end;
		_ ->
		    yang_json:data_to_json(Elems, Env, Data)
	    end
    end.

enum_descr(false, _) -> false;
enum_descr({leaf, _, _, I}, V) ->
    case lists:keyfind(type, 1, I) of
	{_, _, <<"enumeration">>, I1} ->
	    enum_descr_(I1, V);
	_ ->
	    false
    end.

%% Assume rpc-status can be either the numeric value or the description.
enum_descr_([{enum,_,V,I}|_], V) ->
    case lists:keyfind(description,1,I) of
	{_, _, Descr, _} -> Descr;
	false -> V
    end;
enum_descr_([{enum,_,D,I}|T], V) ->
    case lists:keyfind(value, 1, I) of
	{_, _, V, _} ->
	    case lists:keyfind(description,1,I) of
		{_, _, Descr, _} -> Descr;
		false -> D
	    end;
	_ ->
	    enum_descr_(T, V)
    end;
enum_descr_([_|T], V) ->
    enum_descr_(T, V);
enum_descr_([], _) ->
    false.



find_leaf(K, [{leaf,_,K,_} = L|_]) -> L;
find_leaf(K, [_|T]) -> find_leaf(K, T);
find_leaf(_, []) -> false.

keyfind(A, [H|T]) when is_tuple(H) ->
    K = element(1, H),
    case comp(A,K) of
	true ->
	    H;
	false ->
	    keyfind(A, T)
    end;
keyfind(_, []) ->
    false.

comp(A, A) -> true;
comp(A, B) when is_binary(A), is_list(B) ->
    binary_to_list(A) == B;
comp(A, B) when is_binary(A), is_atom(B) ->
    A == atom_to_binary(B, latin1);
comp(_, _) ->
    false.



post_json(Env, JSON) ->
    try
    {_, DID} = lists:keyfind('device-id', 1, Env),
    {_, AID} = lists:keyfind(aid, 1, Env),
    case [U || {'notification-url', U} <- Env] ++
	exodm_db_device:lookup_group_notifications(AID, DID) of
	[] ->
	    ?debug("No group notifications for (~p, ~p)~n", [AID, DID]),
	    ok;
	[_|_] = URLs ->
	    ?debug("Group notifications (~p, ~p): ~p~n", [AID,DID,URLs]),
	    Body = json2:encode(JSON),
	    Hdrs = [
		    {"Content-Length", integer_to_list(iolist_size(Body))},
		    {"Content-Type", "application/json"},
		    {"Host", "localhost"}
		   ],
	    [post_request(URL, Hdrs, Body) ||
		URL <- remove_duplicate_urls(URLs)],
	    ok
    end
    catch
	error:Err ->
	    ?error("CRASH ~p; ~p~n", [Err, erlang:get_stacktrace()])
    end.


%% Since we can end up with multiple URIs that are virtually identical, as we
%% form the union of related device groups, we do our best to normalize the
%% URIs (anything else we can do except ensure an ending slash?), and then
%% remove duplicates.
remove_duplicate_urls(URLs) ->
    remove_duplicate_urls_([normalize_url(U) || U <- URLs]).
    %% lists:usort([normalize_url(U) || U <- URLs]).

remove_duplicate_urls_([{Un,_EndingSlash,U} = First|URLs]) ->
    case [Match || {Un1,_,_} = Match <- URLs,
		   Un1 == Un] of
	[] ->
	    [U|remove_duplicate_urls_(URLs)];
	[_|_] = Matches ->
	    [pick_url([First|Matches])|remove_duplicate_urls_(URLs -- Matches)]
    end;
remove_duplicate_urls_([]) ->
    [].

pick_url([{_, true, U}|_]) -> U;
pick_url([{_, _, U}|URLs]) -> pick_url(URLs, U).

pick_url([{_, true, U}|_], _) -> U;
pick_url([_|URLs], U) -> pick_url(URLs, U);
pick_url([], U) -> U.


normalize_url(U) ->
    case re:split(U, <<"([@\\?])">>, [{return, binary}]) of
	[Simple] ->
	    {HadSlash, Norm} = ensure_ending_slash(Simple),
	    {Norm, HadSlash, U};
	[Auth, <<"@">>, Path, <<"?">> | Rest] ->
	    {HadSlash, PathNorm} = ensure_ending_slash(Path),
	    Norm = iolist_to_binary(
		     [Auth, <<"@">>, PathNorm, <<"?">> | Rest]),
	    {Norm, HadSlash, U};
	[Auth, <<"@">>, Path] ->
	    {HadSlash, PathNorm} = ensure_ending_slash(Path),
	    Norm = iolist_to_binary([Auth, <<"@">>, PathNorm]),
	    {Norm, HadSlash, U};
	[Path, <<"?">> | Rest] ->
	    {HadSlash, PathNorm} = ensure_ending_slash(Path),
	    Norm = iolist_to_binary([PathNorm, <<"?">> | Rest]),
	    {Norm, HadSlash, U}
    end.

ensure_ending_slash(Bin) ->
    Sz = byte_size(Bin),
    Sz_1 = Sz - 1,
    case Bin of
	<<_P:Sz_1/binary, "/">> ->
	    {true, Bin};
	_ ->
	    {false, <<Bin/binary, "/">>}
    end.

post_request(URL, Hdrs, Body) ->
    try
	Host = get_host_part(URL),
	Hdrs1 = lists:keystore("Host", 1, Hdrs, {"Host", Host}),
	Res =
	    lhttpc:request(
	      binary_to_list(URL), "POST", Hdrs1, Body, 1000),
	?debug("post_request(~p, ...) ->~n  ~p~n", [URL, Res]),
	Res
    catch
	Type:Reason ->
	    ?error("post_request(~p, ~p, ~p) CRASHED~n"
		   "~p:~p; ~p~n",
		   [URL, Hdrs, Body, Type, Reason, erlang:get_stacktrace()]),
	    error
    end.

get_host_part(URL0) ->
    URL = if is_list(URL0) -> URL0;
	     is_binary(URL0) -> binary_to_list(URL0)
	  end,
    #lhttpc_url{host = Host} = lhttpc_lib:parse_url(URL),
    Host.

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


%% EUnit tests

-ifdef(TEST).
-define(_t(E), {timeout,60000,
                [?_test(try E catch error:_R_ ->
                                      error({_R_, erlang:get_stacktrace()})
                              end)]}).
%% ?dbg(E): executes test case, printing the result on success, and a
%% thorough error report on failure.
-define(dbg(E),
        (fun() ->
                 try (E) of
                     __V ->
                         ?debugFmt(<<"~s = ~P">>, [(??E), __V, 15]),
                         __V
                 catch
                     error:__Err ->
                         io:fwrite(user,
                                   "FAIL: test = ~s~n"
                                   "Error = ~p~n"
                                   "Trace = ~p~n", [(??E), __Err,
                                                    erlang:get_stacktrace()]),
                         error(__Err)
                 end
          end)()).
-define(my_t(E), ?_test(?dbg(E))).
all_test_() ->
    [?my_t(test_dupl_url())].

test_dupl_url() ->
    URLs1 = [<<"http://foo:bar@a.b.c">>,
	     <<"http://a.b.c">>,
	     <<"http://a.b.c/x/y">>,
	     <<"http://a.b.c/x/y?x=1">>,
	     <<"http://a.b.c?x=1">>],
    URLs2 = [<<"http://foo:bar@a.b.c/">>,
	     <<"http://a.b.c/">>,
	     <<"http://a.b.c/x/y/">>,
	     <<"http://a.b.c/x/y/?x=1">>,
	     <<"http://a.b.c/?x=1">>],
    URLs1 = remove_duplicate_urls(URLs1 ++ URLs1 ++ URLs1),
    URLs2 = remove_duplicate_urls(URLs1 ++ URLs2),
    URLs2 = remove_duplicate_urls(URLs2 ++ URLs1),
    URLs2 = remove_duplicate_urls(URLs1 ++ URLs2 ++ URLs2),
    URLs2 = remove_duplicate_urls(URLs2 ++ URLs1 ++ URLs1),
    URLs2Rev = lists:reverse(URLs2),
    URLs2Rev = remove_duplicate_urls(lists:reverse(URLs1 ++ URLs2)),
    URLs2Rev = remove_duplicate_urls(lists:reverse(URLs2 ++ URLs1)),
    ok.

-endif.
