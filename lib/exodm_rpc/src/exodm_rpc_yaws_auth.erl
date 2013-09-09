-module(exodm_rpc_yaws_auth).

-export([auth/2]).

-include_lib("yaws/include/yaws_api.hrl").
-include_lib("lager/include/log.hrl").
-include("exodm.hrl").

auth(#arg{headers = Hdrs} = Arg, _RequestBody) ->
    io:requests([{put_chars,
		  [io_lib:format("~p:auth(", [?MODULE]),
		   io_lib_pretty:print(obfuscate(Arg), 1, 80, -1, 60, rpfun()),
		   ")"]},
		  nl]),
    %% We should later be able to determine the user's identity e.g. when
    %% the request arrives over a secure connection. Currently, we only
    %% look at the authentication record.
    case Hdrs#headers.authorization of
	{UserStr, Pwd, "Basic " ++ _I} ->
	    ?debug("Authenticating ~s; pwd: ********...~n", [UserStr]),
            User = list_to_binary(UserStr),
	    Req = Arg#arg.req,
            try 
		case find_account(User, Req) of
		    {user, Aid, UserID} when is_binary(Aid) ->
			?debug("find_account(~p, Req) -> ~p~n",
			       [User, {user,Aid,UserID}]),
			case exodm_db_session:authenticate(Aid, UserID, Pwd) of
			    true -> {true, User};
			    false -> false
			end;
		    {device, Aid, DID} ->
			?debug("find_account(~p, ~p) -> ~p~n",
			       [User, {device,Aid,DID}]),
			case exodm_db_session:authenticate(
			       device, Aid, DID, Pwd) of
			    true -> {true, DID};
			    false -> false
			end;
		    {error, _Reason} ->
			false
		end
	    catch
		error:Reason ->
		    ?error("Crash in ~p: ~p~n",
			   [?MODULE,{Reason,erlang:get_stacktrace()}]),
		    false
	    end;
	Other ->
	    ?debug("authorization: Other = ~p~n", [Other]),
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

find_account(Device, #http_request{path = {abs_path, "/exoport" ++ _}}) ->
    %% URI indicates this is a device
    case exodm_db:dec_ext_key(Device) of
	{AID, ID} ->
	    case exodm_db_device:exist(AID, ID) of
		true ->
		    {device, AID, ID};
		false ->
		    {error, no_such_device}
	    end;
	error ->
	    {error, invalid_device}
    end;
find_account(<<C,_/binary>> = User, _Req) ->
    case lists:member(C, exodm_db:delimiters()) of
	true ->
	    case exodm_db:dec_ext_key(User) of
		{AID, ID} ->
		    Accts = exodm_db_user:list_accounts(ID),
		    case lists:member(AID, Accts) of
			true ->
			    {user, AID, ID};
			false ->
			    {error, invalid_account}
		    end;
		error ->
		    {error, invalid_account}
	    end;
	false ->
	    case exodm_db_user:list_accounts(User) of
		[AID|_] ->
		    {user, AID, User};
		[] ->
		    ?error("No account for user ~p", [User]),
		    {error, no_account}
	    end
    end.
    %%     List ->
    %%         %% FIXME
    %%         %% We might need to handle several AID as well ... 
    %%         %% since a user can belong to several accounts
    %%         %% Not possible to handle now
    %%         %% Parse rpc ??
    %%         ?error("Account not choosen among ~p", [List]),
    %%         {error, account_not_specified}
    %% end.
  

obfuscate(#arg{headers = Hdrs} = Arg) ->
    case Hdrs#headers.authorization of
	{User, _, _} ->
	    Arg#arg{headers =
			Hdrs#headers{authorization =
					 {User, "********", "********"}}};
	_ ->
	    Arg
    end.
