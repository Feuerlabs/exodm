-module(exodm_rpc_yaws_auth).

-export([auth/2]).

-include_lib("yaws/include/yaws_api.hrl").
-include_lib("lager/include/log.hrl").
-include_lib("exodm/include/exodm.hrl").

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
	{UserStr, Pwd, "Basic " ++ _I} ->
	    ?debug("Authenticating ~s...~n", [UserStr]),
            User = list_to_binary(UserStr),
            case find_account(User) of
                Aid when is_binary(Aid) ->
                    case exodm_db_session:authenticate(Aid, User, Pwd) of
                        true -> {true, User};
                        false -> false
                    end;
                {error, _Reason} ->
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

find_account(User) ->
    case exodm_db_user:list_accounts(User) of
        [Aid] ->
            ?debug("account ~p found for user ~p", [Aid, User]),
            Aid;
        [] ->
            ?error("No account for user ~p", [User]),
            {error, no_account};
        List ->
            %% FIXME
            %% We might need to handle several AID as well ... 
            %% since a user can belong to several accounts
            %% Not possible to handle now
            %% Parse rpc ??
            ?error("Account not choosen among ~p", [List]),
            {error, account_not_specified}
    end.
  
