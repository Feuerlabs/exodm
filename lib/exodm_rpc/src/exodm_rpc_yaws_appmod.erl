-module(exodm_rpc_yaws_appmod).
-compile(export_all).

-include_lib("yaws/include/yaws_api.hrl").
-include_lib("lager/include/log.hrl").

-define(GA_CUSTOMER_ID, 16#00000001).

out(#arg{req = #http_request{
	   path = {abs_path, "/exodm/test_callback"}}} = A) ->
    yaws_rpc:handler_session(A, {?MODULE, test_callback});
out(#arg{req = #http_request{
	   path = {abs_path, "/exodm/test_callback2"}}} = A) ->
    yaws_rpc:handler_session(A, {?MODULE, test_callback2});
out(#arg{req = #http_request{
	   path = {abs_path, "/exoport"}}} = A) ->
    exodm_rpc_handler:exoport_handler_session(A);
out(#arg{req = #http_request{method = 'POST'},
         pathinfo = "/rpc"} = A) ->
    ?debug("~p: Redirected request:~n~s~n", [?MODULE, pp_arg(A)]),
    exodm_rpc_handler:handler_session(A);
out(_Other) ->
    ?debug("~p: Unrecognized request:~n~s~n", [?MODULE, pp_arg(_Other)]),
    [{status,404}].

pp_arg(Arg) ->
    Sz = tuple_size(Arg) -1,
    RF = fun(arg, Size) when Size == Sz ->
                 record_info(fields, arg);
            (_, _) ->
                 no
         end,
    io_lib_pretty:print(Arg, RF).


%% handling the ck3/test_callback requests
ck3_callback(_St, {call, 'waypoint-request', Req}, Session) ->
    io:fwrite("Test Callback received waypoint request: ~p~n", [Req]),
    {true, 0, Session, {response,
			{struct, [{"rpc-status", "1"},
				  {"rpc-status-string",
				   "The operation has completed successfully."},
				  {"final", "1"}
				  ]}}};
ck3_callback(_St, {notification,Method,Msg}, _Session) ->
    io:fwrite("Test Callback received notification: ~p, ~p~n", [Method, Msg]),
    false.

test_callback(_St, {notification,Method,Msg}, _Session) ->
    io:fwrite("Test Callback received notification: ~p, ~p~n", [Method, Msg]),
    false.

test_callback2(_St, {notification,Method,Msg}, _Session) ->
    io:fwrite("Test Callback 2 received notification: ~p, ~p~n", [Method, Msg]),
    false.


ck3_rpc(St, Req, Session) ->
    try ck3_rpc_(St, Req, Session)
    catch
	error:E ->
	    io:fwrite("*** ERROR! ~p:ck3_rpc(~p, ~p, ~p)~n"
		      "  E = ~p~n"
		      "  Trace = ~p~n",
		      [?MODULE, St, Req, Session, E, erlang:get_stacktrace()]),
	    error(E)
    end.

ck3_rpc_([{ip, IP}] = _State, {call, Method, Request}, Session) ->
    io:fwrite("cp3_rpc: Method = ~p; Request = ~p~n", [Method, Request]),
    OwnerID = ?GA_CUSTOMER_ID,
    case exodm_ck3_rpc:verify_rpc(Method, Request) of
	{verified, RPC} ->
	    %% _Str = lists:flatten(io_lib:fwrite("~p", [RPC])),
	    case exodm_ck3_rpc:queue_message(
		   ck3_to_device, [{owner_id, OwnerID},
				   {ip, IP}], RPC) of
		{ok, _, _} ->
		    Response = exodm_ck3_rpc:success_response(RPC),
		    {true, 0, Session, {response, Response}};
					%% {struct, [{method, atom_to_list(Method)},
					%% 	  {rpc, Str}]}}};
		{error,_} ->
		    {false, {error, "error_handling_request"}}
	    end;
	not_verified ->
	    %% what to do here?
	    {false, {error, "invalid_request"}}
    end.


counter([{ip, _IP}] = _State, {call, errortest, _Value} = _Request, _Session) ->
    io:format("~w: Request = ~p~n", [?MODULE, _Request]),
    { false, { error, "Expected failure" } };

counter([{ip, IP}] = _State, {call, test1, _Value} = _Request, Session) ->
    io:format("~w: Request = ~p~n", [?MODULE, _Request]),
    IPStr = io_lib:format("Client ip is  ~p~n" , [ IP ]),
    OldSession = io_lib:format("Request is: ~p~nOld session value "
                               "is ~p~n", [ _Request, Session ]),

    case Session of
        undefined -> % create new session
            NewSession = 0;
        10 ->        % reset session after reaching 10
            NewSession = undefined;
        N ->
            NewSession = N + 1
    end,

    NewVal = io_lib:format("New session value is ~p ~n", [ NewSession ]),
    Str = lists:flatten([IPStr,OldSession,NewVal]),
    {true, 0, NewSession, {response,  Str }}.
