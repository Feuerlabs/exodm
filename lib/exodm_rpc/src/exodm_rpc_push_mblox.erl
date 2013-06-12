-module(exodm_rpc_push_mblox).
-behaviour(gen_server).

-export([start_link/0,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-compile(export_all).

-include_lib("kvdb/include/kvdb_conf.hrl").
-include_lib("lager/include/log.hrl").

-record(st, {tokens = dict:new()}).

%% #!/bin/sh
%% #  get_accesstoken: Call mblox to get API token
%% #
%% curl --user <mblox-consumer-key>:<mbox-conumer-secret> \
%%    -d grant_type=client_credentials \
%%     --output token.json     https://api.mblox.com/oauthv2/accesstoken
%% grep access_token token.json | sed -e 's/\"//g' -e 's/,//g' -e 's/ //g' | awk -F: '{ print $2 }' > token



%% #!/bin/sh
%% #    send_sms: Send mblox SMS (use get_accesstoken to get a new access token when needed)
%% #
%% APPID=<mblox-appid>
%% ORIGINATOR=<mblox-registered-phone-number>
%% DESTINATION=$1
%% MESSAGE=$2
%% TOKEN=`cat token`
%% curl -H "Authorization: Bearer $TOKEN" \
%%     -H "Content-Type: application/json" \
%%     -d  "{\"message\": \"$2\",\"destination\" : $DESTINATION,\"originator\" : $ORIGINATOR }" https://api.mblox.com/v1/apps/$APPID/sms/outbound/messages

encode_push_message(_AID, _DID, messages) ->
    Call = base64:encode(bert:to_binary({exoport, ping, []})),
    %% Call = ascii_hex_encode(bert:to_binary({exoport, ping, []})),
    {ok, <<"EXODM-RPC:none: ", Call/binary>>}.

send(Ch, AID, DID, _Protocol, Msg) ->
    gen_server:call(Ch, {send, AID, DID, Msg}).


account_updated(AID) ->
    catch gen_server:cast(?MODULE, {account_updated, AID}).

%% Currently, we make no use of the server. It could be used e.g. to
%% periodically refresh the access tokens.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    S = setup_channels(#st{}),
    {ok, S}.

handle_call(Req, From, S) ->
    try handle_call_(Req, From, S)
    catch
	error:_ ->
	    {reply, error, S}
    end.

handle_call_({send, AID0, DID, Msg}, _, #st{tokens = Ts} = S) ->
    AID = exodm_db:account_id_key(AID0),
    {Res, S1} =
	try get_active_token(AID, Ts) of
	    {ok, Token} ->
		send_request(Token, AID, DID, Msg, S);
	    error ->
		case generate_token(AID) of
		    error ->
			{error, S};
		    Token ->
			send_request(Token, AID, DID, Msg, S)
		end
	catch
	    error:Reason ->
		?debug("Send failed: ~p~n",
		       [{Reason, erlang:get_stacktrace()}]),
		{error, S}
	end,
    {reply, Res, S1};
handle_call_(_Req, _From, S) ->
    {reply, error, S}.

handle_cast({account_updated, AID0}, #st{tokens = Ts} = S) ->
    S1 = try  AID = exodm_db:account_id_key(AID0),
	      case generate_token(AID) of
		  error -> S;
		  Token ->
		      S#st{tokens = dict:store(AID, Token, Ts)}
	      end
	 catch
	     error:Reason ->
		 ?debug("updating account ~p failed: ~p~n",
			[AID0, Reason]),
		 S
	 end,
    {noreply, S1};
handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info({timeout, _, {refresh_token, AID}}, S) ->
    try generate_token_(AID) of
	error ->
	    {noreply, S#st{tokens = dict:erase(AID, S#st.tokens)}};
	Token ->
	    {noreply, S#st{tokens = dict:store(
				      AID, Token, S#st.tokens)}}
    catch
	error:Reason ->
	    ?debug("Refresh (~p) failed: ~p~n", [AID, Reason]),
	    {noreply, S#st{tokens = dict:erase(AID, S#st.tokens)}}
    end;
handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_, _) ->
    ok.

code_change(_, S, _) ->
    {ok, S}.



setup_channels(#st{tokens = Ts0} = S) ->
    Tokens = setup_channels_(kvdb_conf:first_top_key(acct), Ts0),
    S#st{tokens = Tokens}.

setup_channels_({ok, AID}, Ts) ->
    ?debug("setup_channels_({ok, ~p}, Ts)~n", [AID]),
    Next = kvdb_conf:next_at_level(acct, AID),
    try generate_token_(AID) of
	error ->
	    ?debug("Couldn't setup mblox channel for ~p~n", [AID]),
	    setup_channels_(Next, Ts);
	Token ->
	    ?debug("Mblox channel set up for ~p~n", [AID]),
	    Ts1 = dict:store(AID, Token, Ts),
	    setup_channels_(Next, Ts1)
    catch
	error:Reason ->
	    ?debug("channel setup failed for ~p: ~p~n",
		   [AID, Reason]),
	    setup_channels_(Next, Ts)
    end;
setup_channels_(done, Ts) ->
    Ts.

get_active_token(AID, Tokens) ->
    dict:find(AID, Tokens).

store_token(AID0, Token, Tokens) ->
    AID = exodm_db:account_id_key(AID0),
    Tokens1 = dict:store(AID, Token, Tokens),
    catch exodm_rpc_push:add_active_channel(AID, <<"*">>),
    Tokens1.

send_request(Token, AID, DID, Msg, S) ->
    [{_, Origin}] = exodm_db_account:lookup_attr(
		      AID, <<"mblox*originating-msisdn">>),
    [{_, AppID}] = exodm_db_account:lookup_attr(
		     AID, <<"mblox*application-id">>),
    URL = "https://api.mblox.com/v1/apps/" ++ binary_to_list(AppID)
	++ "/sms/outbound/messages",
    [{_, Dest}] = exodm_db_device:lookup_attr(AID, DID, <<"msisdn">>),
    Hdrs = [{'Authorization', "Bearer " ++ Token},
	    {'Content-Type', "application/json"}],
    Data = {struct, [{"message", Msg},
		     {"destination", Dest},
		     {"originator", Origin}]},
    send_req_reply(exo_http:wpost(URL, Hdrs, json2:encode(Data)), AID, S).

send_req_reply({ok, {http_response, _, 401, _, _}, _}, AID, S) ->
    %% Unauthorized
    remove_channel(AID),
    {error, S#st{tokens = dict:erase(AID, S#st.tokens)}};
send_req_reply({ok, {http_response, _, 200, _, _}, Data}, _AID, S) ->
    {ok, {struct, Values}} = json2:decode_string(binary_to_list(Data)),
    {{ok, Values}, S}.


%% get_token(AID) ->
%%     case exodm_db_account:lookup_attribute(AID, <<"mblox*token">>) of
%% 	[] ->
%% 	    generate_token_(AID);
%% 	[{_, Token}] ->
%% 	    Token
%%     end.

generate_token(AID) ->
    try generate_token_(AID)
    catch
	error:Reason ->
	    ?debug("generate_token_(~p) failed: ~p~n",
		   [AID, {Reason, erlang:get_stacktrace()}]),
	    remove_channel(AID),
	    error
    end.

add_channel(AID) ->
    exodm_rpc_push:add_active_channel(AID, <<"*">>).

remove_channel(AID) ->
    catch exodm_rpc_push:remove_active_channel(AID, <<"*">>),
    ok.

generate_token_(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    case kvdb_conf:read_tree(
	   acct,
	   kvdb_conf:join_key(AID, <<"mblox">>)) of
	[] ->
	    %% no mblox access credentials
	    error;
	#conf_tree{tree = T} ->
	    ConsumerKey = get_value(<<"consumer-key">>, T),
	    SecretKey = get_value(<<"secret-key">>, T),
	    Auth = base64:encode(<<ConsumerKey/binary, ":",
				   SecretKey/binary>>),
	    Body = [{"grant_type", "client_credentials"}],
	    Hdrs = [{'Authorization', Auth},
		    {'Content-Type', "application/x-www-form-urlencoded"}],
	    token_request_reply(
	      exo_http:wpost("https://api.mblox.com/oauthv2/accesstoken",
			     Hdrs, Body), AID)
    end.

token_request_reply({ok, {http_response, _, 200, _, _Hdrs}, Data}, AID) ->
    {ok, {struct, Values}} = json2:decode_string(binary_to_list(Data)),
    case lists:keyfind("access_token", 1, Values) of
	{_, TokenS} ->
	    Token = list_to_binary(TokenS),
	    kvdb_conf:write(acct, {kvdb_conf:join_key(
				     [AID, <<"mblox">>, <<"token">>]),
				   [], Token}),
	    add_channel(AID),
	    start_timer(Values, AID),
	    Token;
	false ->
	    error({cannot_fetch_token, Data})
    end;
token_request_reply(Other, _AID) ->
    ?debug("Unexpected POST Reply:~n~p~n", [Other]),
    error(error_refreshing_token).


start_timer(Vals, AID) ->
    case lists:keyfind("expires_in", 1, Vals) of
	{_, Secs0} ->
	    Secs = list_to_integer(Secs0),
	    MSecs = min(max(30, Secs - 30) * 1000, 16#ffffffff),
	    ?debug("Refresh timer set to: ~p msecs~n", [MSecs]),
	    erlang:start_timer(MSecs, self(), {refresh_token, AID});
	false ->
	    ?debug("Cannot set refresh timer; no \"expires_in\".~n", []),
	    error
    end.

get_value(K, [{K, _, V}|_]) ->
    V;
get_value(K, [_|T]) ->
    get_value(K, T).

-ifdef(__not_used).
ascii_hex_encode(Bin) when is_binary(Bin) ->
    << << (to_hex(B))/binary >> || <<B:8/integer>> <= Bin >>.

to_hex(B) when B =< 16 -> <<"0", (to_hex_(B))>>;
to_hex(B) when B > 16, B =< 255 ->
    << (to_hex_(B div 16)), (to_hex_(B rem 16)) >>.

to_hex_(B) when B < 10 -> $0 + B;
to_hex_(B) when B >= 10, B =< 16 -> ($A + (B-10)).
-endif.

