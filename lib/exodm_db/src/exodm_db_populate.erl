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


%% each entity has a unique system-generated id:
%% account: ANNNNNNNN
%% group  : GNNNNNNNN
%% device : DNNNNNNNN
%% user   : UNNNNNNNN

tst1() ->
    exodm_db:in_transaction(fun(_) -> tst1_() end).

tst1_() ->
    {ok, _AID} = exodm_db_account:new(
		   [
		    {name, <<"feuer">>},
		    {admin, [{uname, <<"magnus">>},
			     {alias, <<"love">>},
			     {fullname, <<"Magnus Feuer">>},
			     {password, <<"feuerlabs">>}]}
		   ]),
    {ok, _AID2} = exodm_db_account:new(
		    [
		     {name, <<"wiger">>},
		     {admin, [{uname, <<"ulf">>},
			      {alias, [<<"uffe">>, <<"uwiger">>]},
			      {fullname, <<"Ulf Wiger">>},
			      {password, <<"wiger">>}]}
		     ]).

run_rfzone() ->
    exodm_db:in_transaction(fun(_) -> run_rfzone_() end).

run_rfzone_() ->
    {ok, AID} = create_account(seazone),
    {ok, GID} = create_group(seazone, AID),
    store_rfzone_yang(),
    exodm_db_device:new(AID,
			<<"x00000001">>,
			[
			 {'device-key', <<2,0,0,0,0,0,0,0>>},
			 {'server-key', <<1,0,0,0,0,0,0,0>>},
			 {msisdn, <<"070100000000000">>},
			 {groups, [GID]},
			 {yang, <<"rfzone.yang">>}
			]).


run_iodev() ->
    exodm_db:in_transaction(fun(Db) -> run_iodev_(Db) end).

run_iodev_(Db) ->
    {ok, AID} = create_account(fl),
    {ok, _GID} = create_group(fl, AID),
    store_exoio_yang(Db).


run_ga() ->
    exodm_db:in_transaction(fun(Db) -> run_ga_(Db) end),
    %% Need to restart exodm_ck3_server, so that it authenticate itself, now that the account
    %% has been created.
    supervisor:terminate_child(exodm_ck3_sup, exodm_ck3_server),
    supervisor:restart_child(exodm_ck3_sup, exodm_ck3_server).

create_account(ga) ->
    {ok, _AID} = exodm_db_account:new(
		   [
		    {name, <<"getaround">>},
		    {admin, [
			     {uname, <<"ga">>},
			     {fullname, <<"Getaround">>},
			     {password, <<"wewontechcrunch2011">>}
			    ]}]);
create_account(fl) ->
    {ok, _AID} = exodm_db_account:new(
		   [
		    {name, <<"feuerlabs">>},
		    {admin, [
			     {uname, <<"fl">>},
			     {fullname, <<"FeuerLabs Inc">>},
			     {password, <<"1234">>}
			    ]}]);
create_account(seazone) ->
    {ok, _AID} = exodm_db_account:new(
		   [
		    {name, <<"seazone">>},
		    {admin, [
			     {uname, <<"seazone">>},
			     {fullname, <<"Seazone AB">>},
			     {password, <<"seazone">>}
			    ]}]).

create_group(ga, AID) ->
    {ok, _GID} = exodm_db_group:new(
		   AID, [{name, <<"gagroup">>},
			 {url, "https://ga:wewontechcrunch2011@localhost:8080/exodm/callback"}]);
create_group(fl, AID) ->
    {ok, _GID} = exodm_db_group:new(
		   AID, [{name, <<"flgroup">>},
			 {url, "https://localhost:8080/exodm/callback"}]);
create_group(seazone,AID) ->
    {ok, _GID} = exodm_db_group:new(
		   AID, [{name, <<"seazone">>},
			 {url, "http://localhost:8080/exodm/callback"}]).



create_device(AID, GID, 4711) ->
    exodm_db_device:new(AID,
                        <<"4711">>,
			[
			 {'device-key', <<2,0,0,0,0,0,0,0>>},
			 {'server-key', <<1,0,0,0,0,0,0,0>>},
			 {'protocol', <<"ga_ck3">>},
			 {msisdn, <<"07014711">>},
			 {group, {1, GID}},
			 {yang, <<"rfzone.yang">>}
			]).



run_ga_(Db) ->
    {ok, AID} = create_account(ga),
    exodm_db_account:register_protocol(AID, <<"ga_ck3">>),
    {ok, GID} = create_group(ga, AID),
    store_ck3_yang(Db),
    URL = "https://ga:wewontechcrunch2011@localhost:8000/exodm/test_callback",
    {ok,_} = exodm_db_config:new_config_set(
	       AID, [{name, <<"ck3">>},
		     {yang, <<"ckp.yang">>},
		     {'notification-url', URL},
		     {values, []}]),
    %% {ok,_} = exodm_db_config:new_config_set(
    %% 	       AID, [{name, <<"ck3_exo">>},
    %% 		     {yang, <<"exosense.yang">>},
    %% 		     {'notification-url', URL},
    %% 		     {values, []}]),
    {ok, AID, GID}.

run_ga_tst() ->
    exodm_db:in_transaction(
      fun(Db) ->
	      {ok, AID, GID} = run_ga_(Db),
	      ga_dummy_devices_(AID, GID)
      end).

ga_dummy_devices(AID, GID) ->
    exodm_db:in_transaction(
      fun(_) ->
	      ga_dummy_devices_(AID, GID)
      end).

ga_dummy_devices_(AID, GID) ->
    exodm_db_session:set_auth_as_user(<<"ga">>),
    DIDs = lists:map(
	     fun(DID0) ->
		     DID = devid(DID0),
		     exodm_db_device:new(AID, DID,
					 [{'protocol', <<"ga_ck3">>},
					  {'device-key',<<2,0,0,0,0,0,0,0>>},
					  {'server-key',<<1,0,0,0,0,0,0,0>>},
					  {msisdn,"0701"++integer_to_list(DID0)},
					  {groups, [GID]}
					 ]),
		     exodm_ck3_config:new(AID, DID, candidate, []),
		     exodm_ck3_config:new(AID, DID, running, []),
		     DID
	     end, lists:seq(100, 123)),
    exodm_db_config:add_config_set_members(AID, <<"ck3">>, DIDs),
    exodm_db_config:add_config_set_members(AID, <<"ck3_exo">>, DIDs).

devid(I) when is_integer(I) ->
    list_to_binary(integer_to_list(I)).

store_ck3_yang(Db) ->
    App = exodm_ck3,
    exodm_db_session:set_auth_as_user(<<"ga">>, Db),
    %% store_yang(App, "exosense.yang", "yang/exosense.yang"),
    %% store_yang(App, "ieft-inet-types.yang", "yang/ietf-inet-types.yang"),
    store_yang(App, "ckp.yang", "yang/ckp.yang").
    %% store_yang(ck3, "ckp-cfg.yang", "yang/ckp-cfg.yang").

store_yang(App, F, Path) ->
    {ok, Bin} = file:read_file(
		  filename:join(code:priv_dir(App), Path)),
    exodm_db_yang:write(F, Bin).

store_rfzone_yang() ->
    exodm_db_session:set_auth_as_user(<<"seazone">>),
    {ok, UART} = file:read_file(
		   filename:join(code:priv_dir(nmea_0183), "uart.yang")),
    exodm_db_yang:write("uart.yang", UART),
    {ok, Bin} = file:read_file(
		  filename:join(code:priv_dir(rfzone), "rfzone.yang")),
    exodm_db_yang:write("rfzone.yang", Bin).


store_exoio_yang(Db) ->
    exodm_db_session:set_auth_as_user(<<"fl">>, Db),
    {ok, Bin} = file:read_file(
		  filename:join([code:lib_dir(exoiodev),"yang","exoio.yang"])),
    exodm_db_yang:write("exoio.yang", Bin).
    
run_tony() ->
    exodm_db:in_transaction(fun(_) -> run_tony_() end).

run_tony_() ->
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
				  [{'device-key',<<1:32, DID:32>>},
				   {'server-key',<<2:32, DID:32>>},
				   {msisdn,"07012"++integer_to_list(DID)},
				   {group, {1, 1}}
				  ]),
	      exodm_ck3_config:new(12, DID, candidate, []),
	      exodm_ck3_config:new(12, DID, running, [])
      end, lists:seq(1000, 1099)),
    lists:foreach(
      fun(DID) ->
	      exodm_db_device:new(12, DID,
				  [{'device-key',<<1:32, DID:32>>},
				   {'server-key',<<2:32, DID:32>>},
				   {msisdn,"07012"++integer_to_list(DID)},
				   {group, {2, 2}}
				  ]),
	      exodm_ck3_config:new(12, DID, candidate, []),
	      exodm_ck3_config:new(12, DID, running, [])
      end, lists:seq(1100, 1150)),
    lists:foreach(
      fun(DID) ->
	      exodm_db_device:new(12, DID,
				  [{'device-key',<<1:32, DID:32>>},
				   {'server-key',<<2:32, DID:32>>},
				   {msisdn,"07012"++integer_to_list(DID)},
				   {group, {1, 1}},
				   {group, {2, 2}}
				  ]),
	      exodm_ck3_config:new(12, DID, candidate, []),
	      exodm_ck3_config:new(12, DID, running, [])
      end, lists:seq(1151, 1199)),
    ok.

run_love() ->
    exodm_db:in_transaction(fun(_) -> run_love_() end).

run_love_() ->
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
				  [{'device-key',<<1:32, DID:32>>},
				   {'server-key',<<2:32, DID:32>>},
				   {msisdn,"07013"++integer_to_list(DID)},
				   {group, {1, 1}}
				  ]),
	      exodm_ck3_config:new(13, DID, candidate, []),
	      exodm_ck3_config:new(13, DID, running, [])
      end, lists:seq(2000, 2099)).

run_ulf() ->
    exodm_db:in_transaction(fun(_) -> run_ulf_() end).

run_ulf_() ->
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
				  [{'device-key',<<1:32, DID:32>>},
				   {'server-key',<<2:32, DID:32>>},
				   {msisdn,"07014"++integer_to_list(DID)},
				   {group, {1, 1}}
				  ]),
	      exodm_ck3_config:new(14, DID, candidate, []),
	      exodm_ck3_config:new(14, DID, running, [])
      end, lists:seq(1000, 1049)).

run_marcus() ->
    exodm_db:in_transaction(fun(_) -> run_marcus_() end).

run_marcus_() ->
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
				  [{'device-key',<<1:32, DID:32>>},
				   {'server-key',<<2:32, DID:32>>},
				   {msisdn,"07015"++integer_to_list(DID)},
				   {group, {1, 1}}
				  ]),
	      exodm_ck3_config:new(15, DID, candidate, []),
	      exodm_ck3_config:new(15, DID, running, [])
      end, lists:seq(10000, 10009)).


%%
%% Setup typical test data
%%
%%
%%
run() ->
    %% add users
    exodm_db:transaction(
      fun(_) ->
	      run_ga(),

	      run_tony(),
	      run_love(),
	      run_ulf(),
	      run_marcus(),

	      ok
      end).
