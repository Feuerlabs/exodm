-module(exodm_rpc_yaws_auth).

-export([auth/2]).

-include_lib("yaws/include/yaws_api.hrl").
-include_lib("lager/include/log.hrl").

auth(#arg{headers = Hdrs} = Arg, _RequestBody) ->
    io:requests([{put_chars,
		  [io_lib:format("~p:auth(", [?MODULE]),
		   io_lib_pretty:print(Arg, 1, 80, -1, 60, rpfun()),
		   ")"]},
		  nl]),
		  %% We should later be able to determine the user's identity e.g. when
    %% the request arrives over a secure connection. Currently, we only
    %% look at the authentication record.
    case Hdrs#headers.authorization of
	{User, Pwd, "Basic " ++ I} ->
	    ?debug("Authenticating ~s...~n", [User]),
	    case exodm_db_session:authenticate(User, Pwd) of
		{true, UID, AID} ->
		    {true, list_to_binary(User)};
		false ->
		    false
	    end;
	_ ->
	    false
    end.
    %% Info = lists:zip(record_info(fields,headers), tl(tuple_to_list(Hdrs))),
    %% io:fwrite("~p:auth(~n"
    %% 	      "  Hdrs: ~p)~n", [?MODULE, Info]),
    %% {true, "uwiger"}.

rpfun() ->
    N = tuple_size(#arg{}) -1,
    fun(arg, NoFields) when NoFields == N ->
	    record_info(fields, arg);
       (_, _) -> no
    end.
