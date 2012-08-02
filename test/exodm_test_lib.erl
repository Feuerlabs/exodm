-module(exodm_test_lib).

-export([os_cmd/1,
	 os_cmd/2]).


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
