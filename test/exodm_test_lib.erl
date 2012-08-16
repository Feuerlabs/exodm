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
		  http_accept_loop(L, Port, <<>>)
	  end).

http_accept_loop(L, Port, Buf0) ->
    accept_down(),
    Buf = (catch accept_reset(Buf0)),
    case gen_tcp:accept(L, 500) of
	{ok, S} ->
	    %% io:fwrite(user, "HTTP (~p) accepted~n", [Port]),
	    try Msg = http_recv_loop(S, Port, Buf),
		 await_fetch(Msg)
	    catch
		throw:_ -> ok
	    end,
	    http_accept_loop(L, Port, <<>>);
	{error, timeout} ->
	    http_accept_loop(L, Port, Buf)
    end.

accept_down() ->
    receive {'DOWN', _, _, _, _} ->
	    exit(parent_died)
    after 0 -> ok
    end.

accept_reset(Buf) ->
    receive {From, Ref, reset} ->
	    From ! {Ref, reset},
	    throw(<<>>)
    after 0 -> Buf
    end.

await_fetch(Msg) ->
    receive
	{From, Ref, fetch_content} ->
	    From ! {Ref, Msg};
	{From, Ref, reset} ->
	    From ! {Ref, reset},
	    throw(<<>>);
	{'DOWN', _, _, _, _} ->
	    exit(parent_died)
    end.

http_recv_loop(S, Port, Buf) ->
    receive
	{tcp, S, What} ->
	    %% io:fwrite(user, "HTTP recvd ~p; Buf = ~p~n", [What, Buf]),
	    NewBuf = <<Buf/binary, What/binary>>,
	    case erlang:decode_packet(http, NewBuf, []) of
		{ok, {http_request,_,_,_}, Rest} ->
		    [_, Body] = binary:split(Rest, <<"\r\n\r\n">>),
		    %% io:fwrite(user, "HTTP (~p): ~p~n", [Port, Body]),
		    Body;
		{more, _} ->
		    io:fwrite(user, "HTTP more...~n", []),
		    http_recv_loop(S, Port, NewBuf)
	    end;
	{'DOWN', _, _, _, _} ->
	    exit(parent_died);  % we assume
	{From, Ref, reset} ->
	    io:fwrite(user, "got reset in recv_loop~n", []),
	    throw(<<>>);
	{tcp_closed, S} ->
	    throw(<<>>)
    end.
