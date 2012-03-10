%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    Exoweb init - test
%%% @end
%%% Created :  8 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_populate).

-compile(export_all).
-import(lists, [reverse/1]).
%%
%% Setup typical test data
%%
%%
%%
run() ->
    %% add users

    exodm_db_group:new(12, 1, [{name, "default"},{url,  ""}]),
    exodm_db_group:new(12, 2, [{name, "temp"},
			       {url, "http://www.rogvall.se/exodm_client/temp_reading"}]),
    exodm_db_user:new(12, <<"tony">>,
		      [{name,"tony"},
		       {fullname, "Tony Rogvall"},
		       {phone,"+46702575687"},
		       {email,"tony@rogvall.se"},
		       {access, {1,12,1,rw}},
		       {access, {2,12,2,rw}}
		      ]),
    lists:foreach(
      fun(DID) ->
	      exodm_db_device:new(12, DID, 
				  [{ck,<<1:32, DID:32>>},
				   {sk,<<2:32, DID:32>>},
				   {msisdn,"07012"++integer_to_list(DID)},
				   {group, {1, 1}}
				  ]),
	      exodm_db_config:new(12, DID, candidate, []),
	      exodm_db_config:new(12, DID, running, [])
      end, lists:seq(1000, 1099)),
    lists:foreach(
      fun(DID) ->
	      exodm_db_device:new(12, DID, 
				  [{ck,<<1:32, DID:32>>},
				   {sk,<<2:32, DID:32>>},
				   {msisdn,"07012"++integer_to_list(DID)},
				   {group, {2, 2}}
				  ]),
	      exodm_db_config:new(12, DID, candidate, []),
	      exodm_db_config:new(12, DID, running, [])
      end, lists:seq(1100, 1150)),
    lists:foreach(
      fun(DID) ->
	      exodm_db_device:new(12, DID, 
				  [{ck,<<1:32, DID:32>>},
				   {sk,<<2:32, DID:32>>},
				   {msisdn,"07012"++integer_to_list(DID)},
				   {group, {1, 1}},
				   {group, {2, 2}}
				  ]),
	      exodm_db_config:new(12, DID, candidate, []),
	      exodm_db_config:new(12, DID, running, [])
      end, lists:seq(1151, 1199)),



    exodm_db_group:new(13, 1, [{name, "default"},{url,  ""}]),
    exodm_db_user:new(13, <<"love">>,
		      [{name,"love"},
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
				  [{ck,<<1:32, DID:32>>},
				   {sk,<<2:32, DID:32>>},
				   {msisdn,"07013"++integer_to_list(DID)},
				   {group, {1, 1}}
				  ]),
	      exodm_db_config:new(13, DID, candidate, []),
	      exodm_db_config:new(13, DID, running, [])
      end, lists:seq(2000, 2099)),

    exodm_db_group:new(14, 1, [{name, "default"},{url,  ""}]),
    exodm_db_user:new(14, <<"ulf">>,
		      [{name,"ulf"}, 
		       {fullname, "Ulf Wiger"},
		       {phone,"+46761966190"},
		       {email, "ulf@feuerlabs.com"},
		       {access, {1,14,1,rw}}
		      ]),
    lists:foreach(
      fun(DID) ->
	      exodm_db_device:new(14, DID, 
				  [{ck,<<1:32, DID:32>>},
				   {sk,<<2:32, DID:32>>},
				   {msisdn,"07014"++integer_to_list(DID)},
				   {group, {1, 1}}
				  ]),
	      exodm_db_config:new(14, DID, candidate, []),
	      exodm_db_config:new(14, DID, running, [])
      end, lists:seq(1000, 1049)),


    exodm_db_group:new(15, 1, [{name, "default"},{url,  ""}]),
    exodm_db_user:new(15, <<"marcus">>,
		      [{name,"marcus"},
		       {fullname, "Marcus Taylor"},
		       {phone,"+447736180404"},
		       {email, "marcus@feuerlabs.com"},
		       {access, {1,15,1,rw}}
		      ]),
    lists:foreach(
      fun(DID) ->
	      exodm_db_device:new(15, DID,
				  [{ck,<<1:32, DID:32>>},
				   {sk,<<2:32, DID:32>>},
				   {msisdn,"07015"++integer_to_list(DID)},
				   {group, {1, 1}}
				  ]),
	      exodm_db_config:new(15, DID, candidate, []),
	      exodm_db_config:new(15, DID, running, [])
      end, lists:seq(10000, 10009)),
    
    ok.
    

