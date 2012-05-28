%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    Exoweb init - test
%%% @end
%%% Created :  8 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_populate).

-compile(export_all).
-import(lists, [reverse/1]).

-define(GA_CUSTOMER_ID, 16#00000001).


run_euc() ->
    %% Don't know why this is needed...
    BCryptRes = bcrypt:start(),
    io:fwrite("bcrypt:start() -> ~p~n", [BCryptRes]),
    {ok, AID} = exodm_db_account:new(<<"feuerlabs">>, []),
    {ok, GID} = exodm_db_group:new(
		  AID, [{name, <<"euc">>},
			{url, "http://localhost:8000/exodm/test_callback"}]),
    {ok, _UID} = exodm_db_user:new(
		   AID, <<"euc">>,
		   [
		    {fullname, <<"EUC Demo 2012">>},
		    {'__password', <<"exosense">>},
		    {access, {1,AID,GID,rw}}
		   ]),
    store_rfzone_yang(),
    exodm_db_device:new(AID,
			[
			 {'__ck', <<2,0,0,0,0,0,0,0>>},
			 {'__sk', <<1,0,0,0,0,0,0,0>>},
			 {msisdn, <<"0701$DID">>},
			 {group, {1, GID}},
			 {yang, <<"rfzone.yang">>}
			]).

store_rfzone_yang() ->
    exodm_db_session:set_auth_as_user(<<"euc">>),
    {ok, UART} = file:read_file(
		   filename:join(code:priv_dir(nmea_0183), "uart.yang")),
    exodm_db_yang:write("uart.yang", UART),
    {ok, Bin} = file:read_file(
		  filename:join(code:priv_dir(rfzone), "rfzone.yang")),
    exodm_db_yang:write("rfzone.yang", Bin).

run_ga() ->
    exodm_db_group:new(?GA_CUSTOMER_ID, 1, 
		       [{name, "default"},{url,  ""}]),
    exodm_db_group:new(?GA_CUSTOMER_ID, 2, 
		       [{name, "group1"},
			{url, "http://localhost:8080/ck3/test_callback"}]),
    exodm_db_user:new(?GA_CUSTOMER_ID, <<"ga">>,
		      [{name,"ga"},
		       {'__password', <<"ga">>},
		       {fullname, "Get Around"},
		       {access, {1,?GA_CUSTOMER_ID,1,rw}},
		       {access, {2,?GA_CUSTOMER_ID,2,rw}}
		      ]),
    lists:foreach(
      fun(DID) ->
	      exodm_db_device:new(?GA_CUSTOMER_ID, DID, 
				  [{'__ck',<<2,0,0,0,0,0,0,0>>},
				   {'__sk',<<1,0,0,0,0,0,0,0>>},
				   {msisdn,"0701"++integer_to_list(DID)},
				   {group, {1,2}}
				  ]),
	      exodm_db_config:new(?GA_CUSTOMER_ID, DID, candidate, []),
	      exodm_db_config:new(?GA_CUSTOMER_ID, DID, running, [])
      end, lists:seq(100, 123)).

run_tony() ->
    exodm_db_group:new(12, 1, [{name, "default"},{url,  ""}]),
    exodm_db_group:new(12, 2, [{name, "temp"},
			       {url, "http://www.rogvall.se/exodm_client/temp_reading"}]),
    exodm_db_user:new(12, <<"tony">>,
		      [{name,"tony"},
		       {'__password', <<"tony">>},
		       {fullname, "Tony Rogvall"},
		       {phone,"+46702575687"},
		       {email,"tony@rogvall.se"},
		       {access, {1,12,1,rw}},
		       {access, {2,12,2,rw}}
		      ]),
    lists:foreach(
      fun(DID) ->
	      exodm_db_device:new(12, DID, 
				  [{'__ck',<<1:32, DID:32>>},
				   {'__sk',<<2:32, DID:32>>},
				   {msisdn,"07012"++integer_to_list(DID)},
				   {group, {1, 1}}
				  ]),
	      exodm_db_config:new(12, DID, candidate, []),
	      exodm_db_config:new(12, DID, running, [])
      end, lists:seq(1000, 1099)),   
    lists:foreach(
      fun(DID) ->
	      exodm_db_device:new(12, DID, 
				  [{'__ck',<<1:32, DID:32>>},
				   {'__sk',<<2:32, DID:32>>},
				   {msisdn,"07012"++integer_to_list(DID)},
				   {group, {2, 2}}
				  ]),
	      exodm_db_config:new(12, DID, candidate, []),
	      exodm_db_config:new(12, DID, running, [])
      end, lists:seq(1100, 1150)),
    lists:foreach(
      fun(DID) ->
	      exodm_db_device:new(12, DID, 
				  [{'__ck',<<1:32, DID:32>>},
				   {'__sk',<<2:32, DID:32>>},
				   {msisdn,"07012"++integer_to_list(DID)},
				   {group, {1, 1}},
				   {group, {2, 2}}
				  ]),
	      exodm_db_config:new(12, DID, candidate, []),
	      exodm_db_config:new(12, DID, running, [])
      end, lists:seq(1151, 1199)),
    ok.

run_love() ->
    exodm_db_group:new(13, 1, [{name, "default"},{url,  ""}]),
    exodm_db_user:new(13, <<"love">>,
		      [{name,"love"},
		       {'__password', <<"love">>},
		       {fullname, "Magnus Feuer"},
		       {phone,"+19492947871"},
		       {email, "magnus@feuerlabs.com"},
		       {access, {1,13,1,rw}},
		       %% read access to tony's devices gid=2
		       {access, {2,12,2,r}} 
		      ]),
    lists:foreach(
      fun(DID) ->
	      exodm_db_device:new(13, DID, 
				  [{'__ck',<<1:32, DID:32>>},
				   {'__sk',<<2:32, DID:32>>},
				   {msisdn,"07013"++integer_to_list(DID)},
				   {group, {1, 1}}
				  ]),
	      exodm_db_config:new(13, DID, candidate, []),
	      exodm_db_config:new(13, DID, running, [])
      end, lists:seq(2000, 2099)).

run_ulf() ->    
    exodm_db_group:new(14, 1, [{name, "default"},{url,  ""}]),
    exodm_db_user:new(14, <<"ulf">>,
		      [{name,"ulf"},
		       {'__password', <<"ulf">>},
		       {fullname, "Ulf Wiger"},
		       {phone,"+46761966190"},
		       {email, "ulf@feuerlabs.com"},
		       {access, {1,14,1,rw}}
		      ]),
    lists:foreach(
      fun(DID) ->
	      exodm_db_device:new(14, DID, 
				  [{'__ck',<<1:32, DID:32>>},
				   {'__sk',<<2:32, DID:32>>},
				   {msisdn,"07014"++integer_to_list(DID)},
				   {group, {1, 1}}
				  ]),
	      exodm_db_config:new(14, DID, candidate, []),
	      exodm_db_config:new(14, DID, running, [])
      end, lists:seq(1000, 1049)).

run_marcus() ->    
    exodm_db_group:new(15, 1, [{name, "default"},{url,  ""}]),
    exodm_db_user:new(15, <<"marcus">>,
		      [{name,"marcus"},
		       {'__password', <<"marcus">>},
		       {fullname, "Marcus Taylor"},
		       {phone,"+447736180404"},
		       {email, "marcus@feuerlabs.com"},
		       {access, {1,15,1,rw}}
		      ]),
    lists:foreach(
      fun(DID) ->
	      exodm_db_device:new(15, DID,
				  [{'__ck',<<1:32, DID:32>>},
				   {'__sk',<<2:32, DID:32>>},
				   {msisdn,"07015"++integer_to_list(DID)},
				   {group, {1, 1}}
				  ]),
	      exodm_db_config:new(15, DID, candidate, []),
	      exodm_db_config:new(15, DID, running, [])
      end, lists:seq(10000, 10009)).
    
    
%%
%% Setup typical test data
%%
%%
%%
run() ->
    %% add users
    run_ga(),

    run_tony(),
    run_love(),
    run_ulf(),
    run_marcus(),

    ok.
    

