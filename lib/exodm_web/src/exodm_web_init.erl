%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    Exoweb init - test
%%% @end
%%% Created :  8 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_web_init).

-compile(export_all).
-import(lists, [reverse/1]).
%%
%% Setup typical test data
%%
%%
%%
setup() ->
    kvdb:start(),
    DbFile = filename:join(code:priv_dir(exodm_web), "exodm_web.db"),
    kvdb_conf:open(DbFile),
    ok.

add_data() ->
    %% add users
    exodm_db_user:new(12, <<"tony">>,
		      [{name,"tony"},
		       {fullname, "Tony Rogvall"},
		       {phone,"+46702575687"},
		       {email,"tony@rogvall.se"}]),
    
    exodm_db_user:new(13, <<"love">>,
		      [{name,"love"},
		       {fullname, "Magnus Feuer"},
		       {phone,"+19492947871"},
		       {email, "magnus@feuerlabs.com"}]),

    exodm_db_user:new(14, <<"ulf">>,
		      [{name,"ulf"}, 
		       {fullname, "Ulf Wiger"},
		       {phone,"+46761966190"},
		       {email, "ulf@feuerlabs.com"}]),
    
    exodm_db_user:new(15, <<"marcus">>,
		      [{name,"marcus"},
		       {fullname, "Marcus Taylor"},
		       {phone,"+447736180404"},
		       {email, "marcus@feuerlabs.com"}]),
    %% add devices
    lists:foreach(
      fun(DID) ->
	      exodm_db_device:new(12, DID, 
				  [{ck,<<1:32, DID:32>>},
				   {sk,<<2:32, DID:32>>},
				   {msisdn,"07012"++integer_to_list(DID)}]),
	      exodm_db_config:new(12, DID, candidate, []),
	      exodm_db_config:new(12, DID, running, [])
      end, lists:seq(1000, 1200)),

    lists:foreach(
      fun(DID) ->
	      exodm_db_device:new(13, DID, 
				  [{ck,<<1:32, DID:32>>},
				   {sk,<<2:32, DID:32>>},
				   {msisdn,"07013"++integer_to_list(DID)}]),
	      exodm_db_config:new(13, DID, candidate, []),
	      exodm_db_config:new(13, DID, running, [])
      end, lists:seq(2000, 2100)),

    lists:foreach(
      fun(DID) ->
	      exodm_db_device:new(14, DID, 
				  [{ck,<<1:32, DID:32>>},
				   {sk,<<2:32, DID:32>>},
				   {msisdn,"07014"++integer_to_list(DID)}]),
	      exodm_db_config:new(14, DID, candidate, []),
	      exodm_db_config:new(14, DID, running, [])
      end, lists:seq(1000, 1050)),

    lists:foreach(
      fun(DID) ->
	      exodm_db_device:new(15, DID,
				  [{ck,<<1:32, DID:32>>},
				   {sk,<<2:32, DID:32>>},
				   {msisdn,"07015"++integer_to_list(DID)}]),
	      exodm_db_config:new(15, DID, candidate, []),
	      exodm_db_config:new(15, DID, running, [])
      end, lists:seq(10000, 10010)),
    
    ok.
    

