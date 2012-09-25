-module(exodm_test_lib).

-export([os_cmd/1,
	 os_cmd/2,
	 http_server/1,
	 ask_http_servers/2]).


os_cmd(C) ->
    os_cmd(C, print).

os_cmd(C, OutP) when OutP == print; OutP == collect; element(1,OutP)==print ->
    Me = self(),
    {P, Ref} = spawn_monitor(
		 fun() ->
			 Port = open_port({spawn_executable, "/bin/sh"},
					  [{args, ["-c", C]},
					   stream, binary, exit_status,
					   use_stdio, stderr_to_stdout]),
			 get_data(Port, Me, OutP)
		 end),
    receive
	{'DOWN', Ref, _, _, R} ->
	    error(R);
	{P, Status, Data} ->
	    erlang:demonitor(Ref),
	    {Status, Data}
    end.

get_data(Port, Parent, print) ->
    print_loop(Port, Parent, group_leader());
get_data(Port, Parent, {print, P}) ->
    print_loop(Port, Parent, P);
get_data(Port, Parent, collect) ->
    collect_loop(Port, Parent, <<>>).

print_loop(Port, Parent, Dev) ->
    receive
	{Port, {data, Data}} ->
	    io:fwrite(Dev, "~s", [Data]),
	    print_loop(Port, Parent, Dev);
	{Port, {exit_status, St}} ->
	    Parent ! {self(), St, <<>>}
    end.

collect_loop(Port, Parent, Acc) ->
    receive
	{Port, {data, Data}} ->
	    collect_loop(Port, Parent, <<Acc/binary, Data/binary>>);
	{Port, {exit_status, St}} ->
	    Parent ! {self(), St, Acc}
    end.


ask_http_servers(Req, Cfg) ->
    [{Port, ask(Pid, Req)} || {http, {Port,Pid}} <- Cfg].

ask(Pid, Req) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, Req},
    receive
	{Ref, Reply} ->
	    erlang:demonitor(Ref),
	    Reply;
	{'DOWN', Ref, _, _, Reason} ->
	    error(Reason)
    after 10000 ->
	    error(timeout)
    end.

http_server(Port) ->
    Parent = self(),
    spawn(fun() ->
		  _Ref = erlang:monitor(process, Parent),
		  {ok, L} = gen_tcp:listen(Port, [binary,{active,true}]),
		  %% io:fwrite("HTTP (~p) listen socket~n", [Port]),
		  http_accept_loop(L, Port, <<>>, state0())
	  end).

state0() ->
    [].

http_accept_loop(L, Port, Buf0, St0) ->
    accept_down(),
    {Buf, St} = accept_reset(Buf0, St0),
    case gen_tcp:accept(L, 500) of
	{ok, S} ->
	    %% io:fwrite(user, "HTTP (~p) accepted~n", [Port]),
	    St1 = try Msg = http_recv_loop(S, Port, Buf, St),
		       await_fetch(Msg, St)
		  catch
		      throw:reset -> state0();
		      throw:{reset, Opts} -> merge_opts(Opts, St)
		  end,
	    http_accept_loop(L, Port, <<>>, St1);
	{error, timeout} ->
	    http_accept_loop(L, Port, Buf, St)
    end.

accept_down() ->
    receive {'DOWN', _, _, _, _} ->
	    exit(parent_died)
    after 0 -> ok
    end.

accept_reset(Buf, St) ->
    receive
	{From, Ref, reset} ->
	    From ! {Ref, reset},
	    {<<>>, state0()};
	{From, Ref, {reset, Opts}} ->
	    From ! {Ref, reset},
	    {<<>>, merge_opts(Opts, St)}
    after 0 -> {Buf, St}
    end.

await_fetch(Msg, St) ->
    receive
	{From, Ref, fetch_content} ->
	    From ! {Ref, Msg}, St;
	{From, Ref, reset} ->
	    io:fwrite(user, "RESET in await_fetch()~n", []),
	    From ! {Ref, reset},
	    throw(reset);
	{From, Ref, {reset, Opts}} ->
	    io:fwrite(user, "RESET in await_fetch()~n", []),
	    From ! {Ref, reset},
	    throw({reset,Opts});
	{'DOWN', _, _, _, _} ->
	    exit(parent_died)
    end.

merge_opts(Opts, St) ->
    lists:foldl(fun({K,V}, Acc) ->
			lists:keystore(K,1,Acc,{K,V})
		end, St, Opts).


http_recv_loop(S, Port, Buf, St) ->
    receive
	{tcp, S, What} ->
	    tcp_received(What, S, Port, Buf, St);
	{'DOWN', _, _, _, _} ->
	    exit(parent_died);  % we assume
	{From, Ref, reset} ->
	    io:fwrite(user, "RESET in recv_loop~n", []),
	    From ! {Ref, reset},
	    throw(reset);
	{From, Ref, {reset, Opts}} ->
	    From ! {Ref, reset},
	    io:fwrite(user, "RESET in recv_loop~n", []),
	    throw({reset, Opts});
	{tcp_closed, S} ->
	    throw(reset)
    end.

tcp_received(What, S, Port, Buf, St) ->
    %% io:fwrite(user, "HTTP recvd ~p; Buf = ~p~n", [What, Buf]),
    NewBuf = <<Buf/binary, What/binary>>,
    case erlang:decode_packet(http, NewBuf, []) of
	{ok, {http_request,_,_,_}, Rest} ->
	    [_, Body] = binary:split(Rest, <<"\r\n\r\n">>),
	    %% io:fwrite(user, "HTTP (~p): ~p~n", [Port, Body]),
	    case proplists:get_value(http_reply, St) of
		undefined -> no_reply;
		F when is_function(F, 1) ->
		    Rep = F(Body),
		    send_reply(S, Rep)
	    end,
	    Body;
	{more, _} ->
	    io:fwrite(user, "HTTP more...~n", []),
	    http_recv_loop(S, Port, NewBuf, St)
    end.


send_reply(S, Reply) ->
    gen_tcp:send(S, Reply),
    gen_tcp:close(S).
