-module(exodm_setup).

-export([create_db/0,
	 create_tables/0,
	 initial_data/0]).


create_db() ->
    io:fwrite("~p:create_db() ...does nothing.~n", [?MODULE]),
    ok.

create_tables() ->
    io:fwrite("~p:create_tables() ...does nothing.~n", [?MODULE]),
    ok.

initial_data() ->
    io:fwrite("~p:initial_data() ... does nothing.~n", [?MODULE]),
    ok.
