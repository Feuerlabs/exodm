-module(exodm_rpc_yaws_auth).

-export([auth/2]).

-include_lib("yaws/include/yaws_api.hrl").
-include_lib("lager/include/log.hrl").

auth(#arg{headers = Hdrs}, _RequestBody) ->
    %% We should later be able to determine the user's identity e.g. when
    %% the request arrives over a secure connection. Currently, we only
    %% look at the authentication record.
    case Hdrs#headers.authorization of
	{User, Pwd, "Basic " ++ I} ->
	    ?info("Authenticating ~s (Basic ~s)...~n", [User, I]),
	    case exodm_db_session:authenticate(User, Pwd) of
		{true, _UID, _AID} ->
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
