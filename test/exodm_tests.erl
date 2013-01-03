-module(exodm_tests).

-ifdef(TEST).

%% Must explicitly export the RPC callbacks, since otherwise the access check
%% will fail.
-export([test_echo/1,
	 push_config_set_meth/1]).

-include_lib("eunit/include/eunit.hrl").
-include("feuerlabs_eunit.hrl").

-include_lib("parse_trans/include/codegen.hrl").
-include_lib("kvdb/include/kvdb_conf.hrl").

-define(my_t(E), ?_t(?dbg(E))).
-define(rpc(M,F,A), rpc(Cfg, M, F, A)).

-define(loglevels, '__log_levels').
-define(loglevel(L,E),
	push_loglevel(Cfg),
	try  set_loglevel(L, Cfg),
	     E
	after
	    pop_loglevel(Cfg)
	end).

-define(URL1, <<"http://localhost:8898">>).
-define(URL2, <<"http://localhost:8899">>).



exodm_test_() ->
    {setup,
     fun() ->
	     try
		 {ok, CWD} = file:get_cwd(),
		 Top = filename:dirname(CWD),
		 make(Top),
		 Cfg = start_exodm(Top),
		 ok = await_started(Cfg),
		 ok = store_os_pid(Cfg),
		 ok = set_loglevel(Cfg),
		 Cfg
	     catch
		 error:E ->
		     error({E, erlang:get_stacktrace()})
	     end
     end,
     fun(Config) ->
	     ?debugVal(stop_exodm(Config))
     end,
     fun(Config) -> [
		     ?my_t(populate(Config)),
		     ?my_t(list_accounts(Config)),
		     ?my_t(store_config(Config)),
		     ?my_t(store_config2(Config)),
		     ?my_t(list_groups(Config)),
		     ?my_t(list_group_devices(Config)),
		     ?my_t(list_group_notifications(Config)),
		     ?my_t(list_users(Config)),
		     ?my_t(store_yang(Config)),
		     ?my_t(add_config_set_member(Config)),
		     {setup,
		      fun() -> start_http_client(Config) end,
		      fun(Cfg1) -> stop_http_client(Cfg1) end,
		      fun(Cfg1) ->
			      [
			       ?my_t(json_create_device_type(Cfg1)),
			       ?my_t(json_update_device_type(Cfg1)),
			       ?my_t(json_list_device_types(Cfg1)),
			       ?my_t(json_delete_device_type(Cfg1)),
			       ?my_t(json_delete_nonempty_device_type(Cfg1)),
			       ?my_t(json_provision_device(Cfg1)),
			       ?my_t(json_list_devices(Cfg1)),
			       ?my_t(json_list_devices2(Cfg1)),
			       ?my_t(json_list_device_type_members(Cfg1)),
			       ?my_t(json_lookup_device(Cfg1)),
			       ?my_t(json_deprovision_device(Cfg1)),
			       ?my_t(json_lookup_device2(Cfg1)),
			       ?my_t(json_lookup_bad_device(Cfg1)),
			       ?my_t(json_create_config_set(Cfg1)),
			       ?my_t(json_create_config_set3(Cfg1)),
			       ?my_t(json_delete_config_set3(Cfg1)),
			       ?my_t(json_update_config_set(Cfg1)),
			       ?my_t(json_list_config_sets(Cfg1)),
			       ?my_t(json_list_config_sets_dev(Cfg1)),
			       ?my_t(json_create_device_group(Cfg1)),
			       ?my_t(json_list_device_groups(Cfg1)),
			       ?my_t(json_list_device_groups_dev(Cfg1)),
			       ?my_t(json_add_device_group_members(Cfg1)),
			       ?my_t(json_list_device_group_members(Cfg1)),
			       ?my_t(json_remove_device_group_members(Cfg1)),
			       ?my_t(json_list_device_group_members2(Cfg1)),
			       ?my_t(json_delete_device_group(Cfg1)),
			       ?my_t(json_list_device_groups2(Cfg1)),
			       ?my_t(json_add_config_set_members(Cfg1)),
			       ?my_t(json_list_config_set_members(Cfg1)),
			       ?my_t(json_remove_config_set_members(Cfg1)),
			       ?my_t(json_list_config_set_members2(Cfg1)),
			       %% ?my_t(json_delete_config_set(Cfg1)),
			       {setup,
			       	fun() -> start_rpc_client(Config) end,
			       	fun(Cfg2) ->
			       		stop_rpc_client(Cfg2)
			       	end,
			       	fun(Cfg2) ->
			       		%% Here, we have a BERT RPC client
			       		%% (and HTTP client) up and running
			       		[
			       		 ?my_t(client_ping(Cfg2)),
			       		 ?my_t(test_notification(Cfg2)),
			       		 ?my_t(device_json_rpc1(Cfg2)),
			       		 ?my_t(push_config_set1(Cfg2))
			       		]
			       	end}
			      ]
		      end}
		    ]
     end}.

%% Tests

%% for pasting into the shell during interactive debugging:
%% exodm_db_account:new([{name,<<"test">>},{admin,[{uname,<<"t">>},{alias,<<"tee">>},{fullname,<<"Mr T">>},{password,<<"pwd">>}]}]).
%%
populate(Cfg) ->
    {ok, AID1} = ?rpc(exodm_db_account,new,
		      [
		       [
			{name, <<"feuer">>},
			{admin, [{uname, <<"magnus">>},
				 {alias, <<"love">>},
				 {fullname, <<"Magnus Feuer">>},
				 {password, <<"feuerlabs">>}]}
		       ]]),
    {ok, AID2} = ?rpc(exodm_db_account,new,
		      [
		       [
			{name, <<"wiger">>},
			{admin, [{uname, <<"ulf">>},
				 {alias, [<<"uffe">>, <<"uwiger">>]},
				 {fullname, <<"Ulf Wiger">>},
				 {password, <<"wiger">>}]}
		       ]]),
    {ok, GID1} = ?rpc(exodm_db_group,new,
		      [
		       AID2, [{name, <<"feuerlabs">>},
			      {url, ?URL1}]
    		     ]),
    {ok, GID2} = ?rpc(exodm_db_group,new,
    		     [
    		      AID2, [{name, <<"travelping">>},
    			     {url, ?URL2}]
    		     ]),
    ok = ?rpc(exodm_db_device_type, new,
	      [AID1, <<"type1">>,
	       [{'protocol', <<"exodm_bert">>}]]),
    ok = ?rpc(exodm_db_device_type, new,
	      [AID2, <<"type1">>,
	       [{'protocol', <<"exodm_bert">>}]]),
    ok = ?rpc(exodm_db_device, new,
	      [AID2, DID1 = <<"x00000001">>,
	       [
		%% {'protocol', <<"exodm_bert">>},
		{'device-type', <<"type1">>},
		{'device-key',<<2,0,0,0,0,0,0,0>>},
		{'server-key',<<1,0,0,0,0,0,0,0>>},
		{msisdn,"070100000001"},
		{groups, [GID1, GID2]}
	       ]]),
    ok = ?rpc(exodm_db_device, new,
	      [AID2, DID2 = <<"x00000002">>,
	       [
		%% {'protocol', <<"exodm_bert">>},
		{'device-type', <<"type1">>},
		{'device-key',<<2,0,0,0,0,0,0,0>>},
		{'server-key',<<1,0,0,0,0,0,0,0>>},
		{msisdn,"070100000002"},
		{groups, [GID1, GID2]}
	       ]]),
    ok = ?rpc(exodm_db_device, new,
	      [AID2, DID3 = <<"x00000003">>,
	       [
		%% {'protocol', <<"exodm_bert">>},
		{'device-type', <<"type1">>},
		{'device-key',<<2,0,0,0,0,0,0,0>>},
		{'server-key',<<1,0,0,0,0,0,0,0>>},
		{msisdn,"070100000003"},
		{groups, [GID1]}
	       ]]),
    ?debugFmt("AID1 = ~p; AID2 = ~p;~n"
	      "DID1 = ~p; DID2 = ~p; DID3 = ~p~n",
	      [AID1, AID2, DID1, DID2, DID3]),
    ok.

list_users(Cfg) ->
    [<<"magnus">>, <<"ulf">>] = ?rpc(exodm_db_user, list_user_keys, []),
    U = [{name,<<"ulf">>},
	 {'__aid',2},
	 {fullname,<<"Ulf Wiger">>},
	 {phone,<<>>},
	 {email,<<>>},
	 {skype,<<>>}],
    U = ?rpc(exodm_db_user, lookup, [<<"ulf">>]),
    U = ?rpc(exodm_db_user, lookup_by_alias, [<<"uffe">>]),
    ok.

list_accounts(Cfg) ->
    [<<"a00000001">>,
     <<"a00000002">>] = rpc(Cfg, exodm_db_account, list_account_keys, []),
    [{id,<<"00000001">>},
     {name, <<"feuer">>}] =
	rpc(Cfg, exodm_db_account, lookup, [<<"a00000001">>]),
    [{id,<<"00000002">>},
     {name, <<"wiger">>}] =
	rpc(Cfg, exodm_db_account, lookup, [<<"a00000002">>]),
    ok.

list_groups(Cfg) ->
    [<<"g00000001">>,
     <<"g00000002">>] =
	?rpc(exodm_db_account,list_groups, [2]),
    ok.

list_group_devices(Cfg) ->
    [<<"x00000001">>,
     <<"x00000002">>,
     <<"x00000003">>] =
	?rpc(exodm_db_group, list_devices, [2,1]), % AID=2, GID=1
    [<<"x00000001">>,
     <<"x00000002">>] =
	?rpc(exodm_db_group, list_devices, [2,2]), % AID=2, GID=2
    [<<"x00000001">>] =
	?rpc(exodm_db_group, list_devices, [2,2,1,<<>>]), % list 1, prev= <<>>
    [<<"x00000002">>,
     <<"x00000003">>] =
	?rpc(exodm_db_group, list_devices, [2,1,99,<<"x00000001">>]),
    ok.

list_group_notifications(Cfg) ->
    %% [<<"https://ulf:wiger@localhost:8000/exodm/test_callback2">>,
    %%  <<"https://ulf:wiger@localhost:8000/exodm/test_callback">>] =
    [?URL1, ?URL2] =
	?rpc(exodm_db_device,lookup_group_notifications, [2, <<"x00000001">>]),
    ok.


store_yang(Cfg) ->
    ok = rscript(Cfg, store_yang_scr()).

store_yang_scr() ->
    codegen:exprs(
      fun() ->
	      exodm_db:transaction(
		fun(Db) ->
			exodm_db_session:set_auth_as_user(<<"ulf">>, Db),
			{ok, Bin} = file:read_file(
				      filename:join(
					filename:dirname(
					  filename:dirname(setup:home())),
					"test/test.yang")),
			exodm_db_yang:write("test.yang", Bin),
			exodm_db_session:logout()
		end),
	      ok
      end).


store_config(Cfg) ->
    R = <<"test1">>,
    {ok, R} =
	rscript(Cfg, store_config_scr()),
    T = ?rpc(exodm_db_config, read_config_set, [<<"a00000002">>,<<"test1">>]),
    {T,T} = {T, [{name, <<"test1">>},
		 {yang, <<"ckp-cfg.yang">>},
		 {'notification-url',?URL1}]},
		%%  {<<"values">>, [{<<"cksrv-address">>, [], <<"127.0.0.1">>},
		%% 		 {<<"kill-switch">>, [], 0},
		%% 		 {<<"wakeup-prof">>,
		%% 		  [{1, [{<<"data">>, [], <<"01010102">>},
		%% 			{<<"id">>, [], 1}]},
		%% 		   {2, [{<<"data">>, [], <<"01010103">>},
		%% 			{<<"id">>, [], 2}]}]}
		%% 		]},
		%%  {<<"yang">>, [], <<"ckp-cfg.yang">>}
		%% ]},
    ok.

store_config_scr() ->
    codegen:exprs(
      fun() ->
	      exodm_db:transaction(
		fun(Db) ->
			exodm_db_session:set_auth_as_user(<<"ulf">>, Db),
			AID = exodm_db_session:get_aid(),
			{ok, <<"test1">>} =
			    exodm_db_config:new_config_set(
			      AID,
			      [{name, <<"test1">>},
			       {yang, <<"ckp-cfg.yang">>},
			       {'notification-url', ?URL1},
			       {values,
				{struct,
				 [{<<"kill-switch">>, 0},
				  {<<"cksrv-address">>, <<"127.0.0.1">>},
				  {<<"wakeup-prof">>,
				   {array, [
					    {struct,
					     [{<<"id">>,1},
					      {<<"data">>, <<"01010102">>}]},
					    {struct,
					     [{<<"id">>,2},
					      {<<"data">>, <<"01010103">>}]}
					   ]}
				  }]}}])
		end)
      end).

store_config2(Cfg) ->
    R = <<"test2">>,
    {ok, R} =
	rscript(Cfg, store_config_scr2()),
    T = ?rpc(exodm_db_config, read_config_set, [<<"a00000002">>,<<"test2">>]),
    {T,T} = {T, [{name, <<"test2">>},
		 {yang, <<"test.yang">>},
		 {'notification-url', ?URL2}
		]},
    ok.

store_config_scr2() ->
    codegen:exprs(
      fun() ->
	      exodm_db:transaction(
		fun(Db) ->
			exodm_db_session:set_auth_as_user(<<"ulf">>, Db),
			AID = exodm_db_session:get_aid(),
			{ok, <<"test2">>} =
			    exodm_db_config:new_config_set(
			      AID,
			      [{name, <<"test2">>},
			       {yang, <<"test.yang">>},
			       {'notification-url', ?URL2}])
		end)
      end).

add_config_set_member(Cfg) ->
    ok = rscript(Cfg, add_config_set_member_scr()),
    [<<"test1">>, <<"test2">>] =
	?rpc(exodm_db_device, list_config_sets, [<<"a00000002">>,
						 <<"x00000001">>]),
    ok.

add_config_set_member_scr() ->
    codegen:exprs(
      fun() ->
	      exodm_db:transaction(
		fun(Db) ->
			exodm_db_session:set_auth_as_user(<<"ulf">>, Db),
			AID = exodm_db_session:get_aid(),
			exodm_db_config:add_config_set_members(
			  AID, <<"test1">>, [<<"x00000001">>,
					     <<"x00000002">>,
					     <<"x00000003">>]),
			exodm_db_config:add_config_set_members(
			  AID, <<"test2">>, [<<"x00000001">>,
					     <<"x00000002">>])
		end)
      end).

%%% ==================================== Client BERT RPC Setup

start_rpc_client(Cfg) ->
    Auth = ?rpc(exodm_db_device, client_auth_config, [<<"a00000002">>,
						      <<"x00000001">>]),
    ?debugVal(Auth),
    ?assertMatch({true,Auth}, {is_list(Auth), Auth}),
    Apps = [exo, bert, gproc, kvdb, exoport],
    ?debugVal(Apps),
    [{A,ok} = {A, ensure_loaded(A)} || A <- Apps],
    [[application:set_env(A, K, V) || {K,V} <- L] ||
	{A, L} <- [{exoport, [{exodm_address, {"localhost", 9900}},
			      {bert_port, 9990}]} | Auth]],
    [{A,ok} = {A, application:start(A)} || A <- Apps],
    [{client_auth, Auth} | Cfg].

ensure_loaded(A) ->
    case application:load(A) of
	ok -> ok;
	{error, {already_loaded, A}} ->
	    ok;
	Other -> Other
    end.

stop_rpc_client(_Cfg) ->
    Apps = [exoport, kvdb, bert, gproc, exo],
    [{A,ok} = {A, application:stop(A)} || A <- Apps],
    [unload_app(A) || A <- Apps],
    ok.

unload_app(A) ->
    application:unload(A).

%%% ==================================== Client BERT RPC Tests

client_ping(Cfg) ->
    ?loglevel(
       debug,
       {reply, pong, []} = exoport:ping()).

set_access(Cfg) ->
    set_access([{redirect, [{{test,echo,1},
			     {?MODULE,test_echo,1}}]}], Cfg).

set_access(Filter, _Cfg) ->
    {ok, {Host, Port}} = application:get_env(exoport, exodm_address),
    {ok, Sn} = bert_rpc_exec:get_session(
		 Host, Port, [tcp], [{auto_connect,false}], 10000),
    _A = gen_server:call(Sn, get_access),
    ok = gen_server:call(Sn, {set_access, Filter}),
    ok.

test_echo(Args) ->
    io:fwrite(user, "test_echo(~p)~n", [Args]),
    device_json_rpc1 ! {self(), got, Args},
    receive
	{answer, A} ->
	    A
    after 10000 ->
	    error(timeout)
    end.

test_notification(Cfg) ->
    ?loglevel(
       debug,
       notification_(Cfg)).

notification_(Cfg) ->
    ask_http_reset(Cfg),
    exoport:rpc(exodm_rpc, notification, [test, 'echo-callback',
					  [{message, "howdy"}]]),
    Fetched = fetch_json(Cfg),
    Rep = {struct, [{"jsonrpc","2.0"},
		    {"method","test:echo-callback"},
		    {"params", {struct, [{"message", "howdy"}]}}]},
    [{8898, [Rep]}, {8899, [Rep]}] = Fetched,
    ok.


ask_http_reset(Cfg) ->
    ask_http_reset(Cfg, []).

ask_http_reset(Cfg, Opts0) ->
    Opts = default_opt(http_reply, fun(_) -> basic_200_OK() end,
		       default_opt(expect, 1, Opts0)),
    exodm_test_lib:ask_http_servers({reset, Opts}, Cfg).

default_opt(Key, Val, Opts) ->
    case lists:keymember(Key, 1, Opts) of
	true -> Opts;
	false ->
	    [{Key, Val}|Opts]
    end.

basic_200_OK() ->
    <<"HTTP/1.1 200 OK\r\n"
      "Content-Type: text/plain\r\n"
      "Content-Length: 2\r\n\r\n"
      "OK">>.

%%% ==================================== End client BERT RPC


%%% ==================================== JSON-RPC Tests

start_http_client(Cfg) ->
    application:start(crypto),
    application:start(public_key),
    ok = application:start(ssl),
    ok = lhttpc:start(),
    Cfg.

stop_http_client(_Cfg) ->
    ok = lhttpc:stop(),
    ok = application:stop(ssl),
    application:stop(public_key),
    application:stop(crypto).

json_server() ->
    {8000, "ulf", "wiger", "/exodm/rpc"}.

json_create_device_type(_Cfg) ->
    {ok, Reply} = post_json_rpc(json_server(),
				"exodm:create-device-type", 1,
				{struct, [
					  {"name", "devtype_1"},
					  {"protocol", "exodm_bert"}
					 ]}),
    io:fwrite(user, "~p: Reply = ~p~n", [?LINE, Reply]),
    {struct, [{"result", {struct, [{"result",0}]}},
	      {"id", 1},
	      {"jsonrpc", "2.0"}]} = Reply,
    ok.

json_update_device_type(_Cfg) ->
    {ok, Reply} = post_json_rpc(json_server(),
				"exodm:update-device-type", 1,
				{struct, [
					  {"name", "devtype_1"},
					  {"protocol", "ga_ck3"}
					 ]}),
    io:fwrite(user, "~p: Reply = ~p~n", [?LINE, Reply]),
    {struct, [{"result", {struct, [{"result",0}]}},
	      {"id", 1},
	      {"jsonrpc", "2.0"}]} = Reply,
    ok.


json_list_device_types(_Cfg) ->
    {ok, Reply} = post_json_rpc(json_server(),
				"exodm:list-device-types", 1,
				{struct, [
					  {"n", 3},
					  {"previous", ""}
					 ]}),
    io:fwrite(user, "~p: Reply = ~p~n", [?LINE, Reply]),
    {struct, [{"result", {struct, [{"device-types",
				    {array,
				     [
				      {struct, [{"name", "devtype_1"},
						{"protocol", "ga_ck3"}
					       ]},
				      {struct, [{"name", "type1"},
						{"protocol", "exodm_bert"}
					       ]}
				     ]}}
				  ]}},
	      {"id", 1},
	      {"jsonrpc", "2.0"}
	     ]} = Reply,
    ok.

json_delete_device_type(_Cfg) ->
    {ok, Reply} = post_json_rpc(json_server(),
				"exodm:delete-device-type", 1,
				{struct,[{"name", "devtype_1"}]}),
    io:fwrite(user, "~p: Reply = ~p~n", [?LINE, Reply]),
    {struct, [{"result", {struct,[{"result",0}]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.

json_delete_nonempty_device_type(_Cfg) ->
    {ok, Reply} = post_json_rpc(json_server(),
				"exodm:delete-device-type", 1,
				{struct,[{"name", "type1"}]}),
    io:fwrite(user, "~p: Reply = ~p~n", [?LINE, Reply]),
    {struct, [{"result", {struct,[{"result",6}]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.


json_provision_device(_Cfg) ->
    {ok, Reply} = post_json_rpc(json_server(),
				"exodm:provision-device", 1,
				{struct,[{"dev-id", "y00000001"},
					 %% {"protocol", "exodm_bert"},
					 {"device-type", "type1"},
					 {"server-key", 3},
					 {"device-key", 4}]}),
    io:fwrite(user, "~p: Reply = ~p~n", [?LINE, Reply]),
    {struct, [{"result", {struct,[{"result",0}]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.

json_list_devices(_Cfg) ->
    {ok, Reply} = post_json_rpc(json_server(),
				"exodm:list-devices", 1,
				{struct, [{"n", 3},
					  {"previous", ""}]}),
    io:fwrite(user, "~p: Reply = ~p~n", [?LINE, Reply]),
    {struct, [{"result", {struct, [{"devices",
				    {array, [{struct,[{"dev-id","x00000001"}|_]},
					     {struct,[{"dev-id","x00000002"}|_]},
					     {struct,[{"dev-id","x00000003"}|_]}
					     ]}
				   }]}},
	      {"id", 1},
	      {"jsonrpc", "2.0"}]} = Reply,
    ok.

json_list_devices2(_Cfg) ->
    {ok, Reply} = post_json_rpc(json_server(),
				"exodm:list-devices", 1,
				{struct, [{"n", 1},
					  {"previous", "x00000003"}]}),
    io:fwrite(user, "~p: Reply = ~p~n", [?LINE, Reply]),
    {struct, [{"result", {struct, [{"devices",
				    {array, [{struct,[{"dev-id","y00000001"}|_]}
					     ]}
				   }]}},
	      {"id", 1},
	      {"jsonrpc", "2.0"}]} = Reply,
    ok.

json_list_device_type_members(_Cfg) ->
    {ok, Reply} = post_json_rpc(json_server(),
				"exodm:list-device-type-members", 1,
				{struct, [{"name", "type1"},
					  {"n", 2},
					  {"previous", ""}]}),
    io:fwrite(user, "~p: Reply = ~p~n", [?LINE, Reply]),
    {struct, [{"result",
	       {struct, [
			 {"device-type-members",
			  {array, ["x00000001",
				   "x00000002"]}}
			]}},
	      {"id", 1},
	      {"jsonrpc", "2.0"}]} = Reply,
    ok.

json_lookup_device(_Cfg) ->
    {ok, Reply} = post_json_rpc(json_server(),
				"exodm:lookup-device", "1",
				{struct,[{"dev-id", "y00000001"}]}),
    io:fwrite(user, "~p: lookup-device -> ~p~n", [?LINE, Reply]),
    %% lookup-device doesn't return the server- and device-key attributes
    {struct, [{"result",
	       {struct,[{"result", 0},
			{"devices",
			 {array, [{struct, [{"dev-id", "y00000001"},
					    {"device-type", "type1"},
					    {"session-timeout", 0}
					    %% {"protocol", "exodm_bert"}
					   ]}]}}
		       ]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.

json_deprovision_device(_Cfg) ->
    {ok, Reply} = post_json_rpc(json_server(),
				"exodm:deprovision-devices", 1,
				{struct,[{"dev-id",
					  {array, ["y00000001"]}}]}),
    io:fwrite(user, "~p: Reply = ~p~n", [?LINE, Reply]),
    {struct, [{"result", {struct,[{"result",0}]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.

json_lookup_device2(_Cfg) ->
    {ok, Reply} = post_json_rpc(json_server(),
				"exodm:lookup-device", "1",
				{struct,[{"dev-id", "y00000001"}]}),
    io:fwrite(user, "~p: lookup-device (deleted) -> ~p~n", [?LINE, Reply]),
    %% lookup-device doesn't return the server- and device-key attributes
    {struct, [{"result",
	       {struct,[ {"result", 5},
			 {"devices", {array, []}} ]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.


json_lookup_bad_device(_Cfg) ->
    {ok, Reply} = post_json_rpc({8000, "ulf", "wiger", "/exodm/rpc"},
				"exodm:lookup-device", "1",
				{struct,[{"dev-id", "---------"}]}),
    io:fwrite(user, "~p: lookup-device -> ~p~n", [?LINE, Reply]),
    %% lookup-device doesn't return the server- and device-key attributes
    {struct, [{"result",
	       {struct,[ {"result", 5},
			 {"devices", {array, []}} ]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.


json_create_config_set(Cfg) ->
    json_create_config_set_(Cfg, "test_cfg_1").

json_create_config_set3(Cfg) ->
    json_create_config_set_(Cfg, "test_cfg_3").

json_create_config_set_(_Cfg, Name) ->
    {ok, Reply} = post_json_rpc({8000, "ulf", "wiger", "/exodm/rpc"},
				"exodm:create-config-set", "1",
				{struct,[{"name", Name},
					 {"yang", "exosense.yang"},
					 {"notification-url",?URL1},
					 {"values",{struct,[{"a", "1"},
							    {"b", "xx"}
							   ]}}
					]}),
    io:fwrite(user, "~p: Reply = ~p~n", [?LINE, Reply]),
    {struct, [{"result", {struct,[{"result",0}]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.

json_update_config_set(Cfg) ->
    Name = "test_cfg_2",
    ok = json_create_config_set_(Cfg, Name),
    {ok, Reply} = post_json_rpc({8000, "ulf", "wiger", "/exodm/rpc"},
				"exodm:update-config-set", "1",
				{struct,[{"name", Name},
					 {"values",{struct,[{"c","yy"}]}}
					]}),
    io:fwrite(user, "~p: update-config-set -> ~p~n", [?LINE, Reply]),
    {struct, [{"result", {struct,[{"result",0}]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.

json_list_config_sets(_Cfg) ->
    {ok, Reply} = post_json_rpc({8000, "ulf", "wiger", "/exodm/rpc"},
				"exodm:list-config-sets", "1",
				{struct,[{"n",3},
					 {"previous",""}
					]}),
    io:fwrite(user, "~p: list-config-sets -> ~p~n", [?LINE, Reply]),
    {struct, [{"result",
	       {struct,[{"config-sets",
			 {array, [{struct, [{"name", "test1"} | _]},
				  {struct, [{"name", "test2"} | _]},
				  {struct, [{"name", "test_cfg_1"} | _]}
				  ]}
			}]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.

json_list_config_sets_dev(_Cfg) ->
    {ok, Reply} = post_json_rpc({8000, "ulf", "wiger", "/exodm/rpc"},
				"exodm:list-config-sets", "1",
				{struct,[{"n",3},
					 {"previous",""},
					 {"device-id", "x00000001"}
					]}),
    io:fwrite(user, "~p: list-config-sets -> ~p~n", [?LINE, Reply]),
    {struct, [{"result",
	       {struct,[{"config-sets",
			 {array, [{struct, [{"name", "test1"} | _]},
				  {struct, [{"name", "test2"} | _]}
				  ]}
			}]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.


json_add_config_set_members(_Cfg) ->
    {ok, Reply} = post_json_rpc({8000, "ulf", "wiger", "/exodm/rpc"},
				"exodm:add-config-set-members", "1",
				{struct,[{"name", {array, ["test_cfg_2"]}},
					 {"dev-id", {array, ["x00000001",
							     "x00000002"]}}
					]}),
    io:fwrite(user, "~p: add-config-set-members -> ~p~n", [?LINE, Reply]),
    {struct, [{"result", {struct,[{"result",0}]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.


json_list_config_set_members(_Cfg) ->
    {ok, Reply} = post_json_rpc(json_server(),
				"exodm:list-config-set-members", "1",
				{struct, [{"name", "test_cfg_2"},
					  {"n", 2},
					  {"previous", ""}
					 ]}),
    io:fwrite(user, "~p: list-config-set-members -> ~p~n", [?LINE, Reply]),
    {struct, [{"result",
	       {struct,[{"config-set-members",
			 {array, ["x00000001", "x00000002"]}}]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.

json_remove_config_set_members(_Cfg) ->
    {ok, Reply} = post_json_rpc(json_server(),
				"exodm:remove-config-set-members", 1,
				{struct,[{"name", {array, ["test_cfg_2"]}},
					 {"dev-id", {array, ["x00000001",
							     "x00000002"]}}
					]}),
    io:fwrite(user, "~p: remove-config-set-members -> ~p~n", [?LINE, Reply]),
    {struct, [{"result", {struct, [{"result",0}]}},
	      {"id", 1},
	      {"jsonrpc", "2.0"}]} = Reply,
    ok.

json_list_config_set_members2(_Cfg) ->
    {ok, Reply} = post_json_rpc(json_server(),
				"exodm:list-config-set-members", "2",
				{struct, [{"name", "test_cfg_2"},
					  {"n", 2},
					  {"previous", ""}
					 ]}),
    io:fwrite(user, "~p: list-config-set-members(2) -> ~p~n", [?LINE, Reply]),
    {struct, [{"result",
	       {struct,[{"config-set-members",
			 {array, []}}]}},
	      {"id","2"},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.


json_delete_config_set3(_Cfg) ->
    {ok, Reply} = post_json_rpc({8000, "ulf", "wiger", "/exodm/rpc"},
				"exodm:delete-config-set", "1",
				{struct,[{"name","test_cfg_3"}]}),
    io:fwrite(user, "~p: delete-config-set -> ~p~n", [?LINE, Reply]),
    {struct, [{"result", {struct,[{"result",0}]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.



json_create_device_group(_Cfg) ->
    Name = "test_group_1",
    {ok, Reply} = post_json_rpc({8000, "ulf", "wiger", "/exodm/rpc"},
				"exodm:create-device-group", "1",
				{struct,[{"name", Name},
					 {"notification-url",?URL1}
					]}),
    io:fwrite(user, "~p: create-device-group -> ~p~n", [?LINE, Reply]),
    {struct, [{"result", {struct,[{"result",0},{"gid",3}]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.

json_list_device_groups(_Cfg) ->
    {ok, Reply} = post_json_rpc({8000, "ulf", "wiger", "/exodm/rpc"},
				"exodm:list-device-groups", "1",
				{struct, [{"n", 3},
					  {"previous", 0}
					 ]}),
    io:fwrite(user, "~p: list-device-groups -> ~p~n", [?LINE, Reply]),
    {struct, [{"result",
	       {struct,[{"device-groups",
			 {array, [{struct, [{"gid",1},
					    {"name", "feuerlabs"},
					    {"notification-url",_}]},
				  {struct, [{"gid",2},
					    {"name", "travelping"},
					    {"notification-url",_}]},
				  {struct, [{"gid",3},
					    {"name", "test_group_1"},
					    {"notification-url",_}]}
				 ]}}
			]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.

json_list_device_groups_dev(_Cfg) ->
    {ok, Reply} = post_json_rpc({8000, "ulf", "wiger", "/exodm/rpc"},
				"exodm:list-device-groups", "1",
				{struct, [{"n", 3},
					  {"previous", 0},
					  {"device-id", "x00000002"}
					 ]}),
    io:fwrite(user, "~p: list-device-groups -> ~p~n", [?LINE, Reply]),
    {struct, [{"result",
	       {struct,[{"device-groups",
			 {array, [{struct, [{"gid",1},
					    {"name", "feuerlabs"},
					    {"notification-url",_}]},
				  {struct, [{"gid",2},
					    {"name", "travelping"},
					    {"notification-url",_}]}
				 ]}}
			]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.



json_add_device_group_members(_Cfg) ->
    {ok, Reply} = post_json_rpc({8000, "ulf", "wiger", "/exodm/rpc"},
				"exodm:add-device-group-members", "1",
				{struct,[{"device-groups", {array, [3]}},
					 {"dev-id", {array, ["x00000001",
							     "x00000002"]}}
					]}),
    io:fwrite(user, "~p: add-device-group-members -> ~p~n", [?LINE, Reply]),
    {struct, [{"result", {struct,[{"result",0}]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.


json_list_device_group_members(_Cfg) ->
    {ok, Reply} = post_json_rpc({8000, "ulf", "wiger", "/exodm/rpc"},
				"exodm:list-device-group-members", "1",
				{struct,[{"gid", 3},
					 {"n", 2},
					 {"previous", ""}]}),
    io:fwrite(user, "~p: list-device-group-members -> ~p~n", [?LINE, Reply]),
    {struct, [{"result",
	       {struct,[{"device-group-members",
			 {array, ["x00000001",
				  "x00000002"]}}
		       ]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.

json_remove_device_group_members(_Cfg) ->
    {ok, Reply} = post_json_rpc({8000, "ulf", "wiger", "/exodm/rpc"},
				"exodm:remove-device-group-members", "1",
				{struct,[{"device-groups", {array, [3]}},
					 {"dev-id", {array, ["x00000001",
							     "x00000002"]}}
					]}),
    io:fwrite(user, "~p: remove-device-group-members -> ~p~n", [?LINE, Reply]),
    {struct, [{"result", {struct,[{"result",0}]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.

json_list_device_group_members2(_Cfg) ->
    {ok, Reply} = post_json_rpc({8000, "ulf", "wiger", "/exodm/rpc"},
				"exodm:list-device-group-members", "1",
				{struct,[{"gid", 3},
					 {"n", 2},
					 {"previous", ""}]}),
    io:fwrite(user, "~p: list-device-group-members -> ~p~n", [?LINE, Reply]),
    {struct, [{"result",
	       {struct,[{"device-group-members",
			 {array, []}}
		       ]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.


json_delete_device_group(_Cfg) ->
    {ok, Reply} = post_json_rpc({8000, "ulf", "wiger", "/exodm/rpc"},
				"exodm:delete-device-group", "1",
				{struct, [{"gid","3"}]}),
    io:fwrite(user, "~p: delete-device-group -> ~p~n", [?LINE, Reply]),
    {struct, [{"result",
	       {struct, [{"result",0}]}},
	       {"id", _},
	       {"jsonrpc", "2.0"}]} = Reply,
    ok.

json_list_device_groups2(_Cfg) ->
    {ok, Reply} = post_json_rpc({8000, "ulf", "wiger", "/exodm/rpc"},
				"exodm:list-device-groups", "1",
				{struct, [{"n", 1},
					  {"previous", 2}
					 ]}),
    io:fwrite(user, "~p: list-device-groups2 -> ~p~n", [?LINE, Reply]),
    {struct, [{"result",
	       {struct,[{"device-groups",
			 {array, []}}
			]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    ok.



device_json_rpc1(Cfg) ->
    set_access(Cfg),
    spawn_link(fun() ->
		       register(device_json_rpc1, self()),
		       receive
			   {Pid, got, Args} ->
			       io:fwrite(user, "Pid got Args = ~p~n", [Args]),
			       Msg = get_value(message, Args, <<"huh?">>),
			       Reply = {notify, 'echo-callback',
					[{message, Msg}]},
			       Pid ! {answer, Reply}
		       end
	       end),
    ask_http_reset(Cfg),
    {ok, Reply} = post_json_rpc({8000, "ulf", "wiger", "/exodm/rpc"},
				"test:echo", "2",
				{struct, [{"device-id", "x00000001"},
					  {"message", "hello"}]}),
    %% io:fwrite(user, "~p: Reply = ~p~n", [?LINE, Reply]),
    {struct, [{"result", {struct, [{"transaction-id",_},
				   {"rpc-status", 0},
				   {"rpc-status-string",
				    "Operation has been accepted" ++ _},
				   {"final",false}]}},
	      {"id",_},
	      {"jsonrpc","2.0"}]} = Reply,
    Fetched = fetch_json(Cfg),
    Notification = {struct, [{"jsonrpc","2.0"},
			     {"method","test:echo-callback"},
			     {"params", {struct,[{"message","hello"}]}}]},
    [{8898, [Notification]}, {8899, [Notification]}] = Fetched,
    ok.

get_value(K, [H|_], _) when element(1, H) == K ->
    element(2, H);
get_value(K, [_|T], Def) ->
    get_value(K, T, Def);
get_value(_, [], Def) ->
    Def.


%% run_trace(Cfg) ->
%%     ok = rscript(Cfg, run_trace_scr()).

%% run_trace_scr() ->
%%     codegen:exprs(
%%       fun() ->
%% 	      dbg:tracer(),
%% 	      dbg:tp(kvdb_leveldb,x),
%% 	      dbg:tp(eleveldb,x),
%% 	      dbg:tp(kvdb_conf,next_at_level,x),
%% 	      dbg:tpl(kvdb_conf,same_parent,x),
%% 	      dbg:p(all,[c]),
%% 	      ok
%%       end).


push_config_set1(Cfg) ->
    set_access([{redirect, [{{exoport_config, push_config_set, 1},
			     {?MODULE, push_config_set_meth, 1}}]}], Cfg),
    spawn_link(fun() ->
		       register(push_config_set1, self()),
		       receive
			   {Pid, got_push_request, Args} ->
			       io:fwrite(user, "Pid got push ~p~n", [Args]),
			       Reply = {notify, 'push-config-set-callback',
					[{'transaction-id', 1},
					 {'rpc-status', <<"complete">>},
					 {final, true}]},
			       Pid ! {answer, Reply}
		       end
	       end),
    ask_http_reset(Cfg),
    {ok, Reply} = post_json_rpc({8000, "ulf", "wiger", "/exodm/rpc"},
				"exodm:push-config-set", "3",
				{struct, [{"name", "test1"}]}),
    {struct, [{"result", {struct, [{"result", 0}]}},
	      {"id", _},
	      {"jsonrpc", "2.0"}]} = Reply,
    Fetched = fetch_json(Cfg),
    [{8898, [N]}, {8899, [N]}] = Fetched,
    {struct, [{"jsonrpc","2.0"},
	      {"method","exodm:push-config-set-callback"},
	      {"params",
	       {struct, [{"transaction-id", _},
			 {"rpc-status", 1},
			 {"rpc-status-string",
			  "The operation has completed" ++ _},
			 {"final", true}]}}]} = N,
    ok.


push_config_set_meth(Args) ->
    push_config_set1 ! {self(), got_push_request, Args},
    receive
	{answer, A} ->
	    A
    after 10000 ->
	    error(timeout)
    end.


fetch_json(Cfg) ->
    [{Port, [ok(json2:decode_string(binary_to_list(Body))) || Body <- Msgs]} ||
	{Port,Msgs} <- exodm_test_lib:ask_http_servers(fetch_content, Cfg)].

ok({ok, Res}) ->
    Res.

post_json_rpc({Port, User, Pwd, Path}, Method, ID, Params) ->
    URL = lists:concat(["https://", User, ":", Pwd, "@localhost:",
			integer_to_list(Port), Path]),
    Body = json2:encode({struct, [{"jsonrpc", "2.0"},
				  {"method", Method},
				  {"id", ID},
				  {"params", Params}]}),
    Res = lhttpc:request(URL, "POST",
			 [{"Content-Length", integer_to_list(
					       iolist_size(Body))},
			  {"Content-Type", "application/json-rpc"},
			  {"Host", "localhost"}],
			 Body, 3000),
    %% io:fwrite(user, "HTTP request Res = ~p~n", [Res]),
    {ok, {{200,_OK},_Hdrs,JSON}} = Res,
    {ok, ok(json2:decode_string(binary_to_list(JSON)))}.

%% Helpers



get_node(Cfg) ->
    {_, Node} = lists:keyfind(node, 1, Cfg),
    Node.

rpc(Cfg, M, F, A) ->
    rpc:call(get_node(Cfg), M, F, A).

%% runs an interpreted script on the target node
rscript(Cfg, Script) ->
    {value, Res, _} = rpc:call(get_node(Cfg), erl_eval, exprs, [Script, []]),
    Res.

try_rpc(N, T, Node, M, F, A) ->
    case rpc:call(Node, M, F, A) of
	{badrpc, _} when N > 1 ->
	    timer:sleep(T),
	    try_rpc_(N-1, T, Node, M, F, A);
	Other ->
	    Other
    end.

try_rpc_(0, _, _, _, _, _) ->
    error(nodedown);
try_rpc_(N, T, Node, M, F, A) when N > 0 ->
    case rpc:call(Node, M, F, A) of
	{badrpc, _} ->
	    timer:sleep(T),
	    try_rpc_(N-1, T, Node, M, F, A);
	Other ->
	    Other
    end.


%% Setup and teardown

await_started(Cfg) ->
    Node = get_node(Cfg),
    await_started(6, Node).

await_started(0, _) ->
    error(timeout);
await_started(N, Node) when N > 0 ->
    case try_rpc(3, 3000, Node, application_controller, get_master, [exodm]) of
	undefined ->
	    io:fwrite(user, "waiting for exodm app (~p)...~n", [N]),
	    timer:sleep(2000),
	    await_started(N-1, Node);
	P when is_pid(P) ->
	    ok
    end.

store_os_pid(Cfg) ->
    Pid = ?rpc(os, getpid, []),
    io:fwrite(user, "store_os_pid(); Pid = ~p~n", [Pid]),
    {dir, D} = lists:keyfind(dir, 1, Cfg),
    ok = file:write_file(
	   File = filename:join(D, "curpid.data"), list_to_binary(Pid)),
    io:fwrite(user, "wrote ~p to file ~p~n", [Pid, File]).


set_loglevel(Cfg) ->
    case os:getenv("LOGLEVEL") of
	L when L=="debug"; L=="info"; L=="crash" ->
	    set_loglevel(list_to_atom(L), Cfg);
	_ ->
	    ok
    end.

set_loglevel(Level, Cfg) ->
    ?rpc(lager, set_loglevel, [lager_console_backend, Level]),
    ok.

get_loglevel(Cfg) ->
    ?rpc(lager, get_loglevel, [lager_console_backend]).

push_loglevel(Cfg) ->
    Cur = get_loglevel(Cfg),
    case get(?loglevels) of
	undefined ->
	    put(?loglevels, [Cur]);
	L when is_list(L) ->
	    put(?loglevels, [Cur|L])
    end.

pop_loglevel(Cfg) ->
    [Prev|L] = get(?loglevels),
    set_loglevel(Prev, Cfg),
    put(?loglevels, L),
    ok.

make(Top) ->
    case os:getenv("EXODM_SKIP_MAKE") of
	Y when Y==false; Y=="0"; Y=="false"  ->
	    in_dir(Top, fun() ->
				run("make release"),
				run("make generate")
			end);
	_ ->
	    ok
    end.

run(Cmd) ->
    io:fwrite(user, "Cmd: ~s~n", [Cmd]),
    case exodm_test_lib:os_cmd(Cmd, {print, user}) of
	{0, _} ->
	    ok;
	{Err, _} ->
	    error({aborted, [Err, Cmd]})
    end.

start_exodm(Top) ->
    {ok, _} = net_kernel:start([exodm_test, longnames]),
    erlang:set_cookie(node(), 'exodm'),
    Dir = filename:absname("exodm_tmp"),
    NodeStr = "exodm_n1@" ++ hostname(),
    case filelib:is_dir(Dir) of
	true ->
	    kill_old(Dir),
	    os:cmd("rm -r " ++ Dir);
	false -> ok
    end,
    ok = file:make_dir(Dir),
    %% _DevRun = filename:join(Top, "devrun"),

    MakeNode = filename:join(Top, "make_node"),
    Rel = filename:join(Top, "rel/exodm"),
    in_dir(Dir, fun() ->
			?debugVal(
			   os:cmd(MakeNode ++ " -target " ++ Dir
				  ++ " -rel " ++ Rel ++ " -- -name " ++ NodeStr)),
			ENV = "ERL_SETUP_LIBS=\"" ++
			    filename:join(Top,"rel/plugins") ++ "\" ",
			?debugVal(
			   os:cmd(ENV ++ Rel ++ "/bin/exodm start"))
		end),
    Http1 = exodm_test_lib:http_server(8898),
    Http2 = exodm_test_lib:http_server(8899),
    [{dir, Dir},
     {node, list_to_atom(NodeStr)},
     {http, {8898,Http1}},
     {http, {8899,Http2}}].

kill_old(Dir) ->
    case file:read_file(filename:join(Dir, "curpid.data")) of
	{ok, <<P/binary>>} ->
	    io:fwrite(user, "kill pid ~p if still around~n", [P]),
	    exodm_test_lib:os_cmd(<<"kill ", P/binary>>, {print,user});
	{error, _} ->
	    ok
    end.

stop_exodm(Cfg) ->
    Node = proplists:get_value(node, Cfg),
    {badrpc, _} = rpc:call(Node, erlang, halt, []),
    {dir, D} = lists:keyfind(dir, 1, Cfg),
    file:delete(filename:join(D, "curpid.data")),
    ok.

hostname() ->
    case {inet_db:gethostname(),inet_db:res_option(domain)} of
	{H,D} when is_list(D), is_list(H),
		   length(D)> 0, length(H)>0 ->
	    H ++ "." ++ D;
	Other ->
	    error({hostname, Other})
    end.

in_dir(D, F) ->
    {ok, CWD} = file:get_cwd(),
    ok = file:set_cwd(D),
    try F()
    after
	file:set_cwd(CWD)
    end.

-endif.
