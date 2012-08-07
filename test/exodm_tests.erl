-module(exodm_tests).

-ifdef(TEST).

-export([test_echo/1]).

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
		     ?my_t(list_groups(Config)),
		     ?my_t(list_group_notifications(Config)),
		     ?my_t(list_users(Config)),
		     ?my_t(store_exosense_yang(Config)),
		     ?my_t(store_yang(Config)),
		     ?my_t(store_exodm_yang(Config)),
		     ?my_t(store_config(Config)),
		     ?my_t(store_config2(Config)),
		     ?my_t(add_config_data_member(Config)),
		     {setup,
		      fun() -> start_http_client(Config) end,
		      fun(Cfg1) -> stop_http_client(Cfg1) end,
		      fun(Cfg1) ->
			      [
			       ?my_t(json_rpc1(Cfg1)),
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
					 ?my_t(device_json_rpc1(Cfg2))
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
			     {url, "https://ulf:wiger@localhost:8000/exodm/test_callback"}]
		     ]),
    {ok, GID2} = ?rpc(exodm_db_group,new,
		     [
		      AID2, [{name, <<"travelping">>},
			     {url, "https://ulf:wiger@localhost:8000/exodm/test_callback2"}]
		     ]),
    ok = ?rpc(exodm_db_device, new,
	      [AID2, DID = <<"x00000001">>,
	       [{'__ck',<<2,0,0,0,0,0,0,0>>},
		{'__sk',<<1,0,0,0,0,0,0,0>>},
		{msisdn,"070100000001"},
		{groups, [GID1, GID2]}
	       ]]),
    ?debugFmt("AID1 = ~p; AID2 = ~p; GID1 = ~p; GID2 = ~p; DID = ~p~n",
	      [AID1, AID2, GID1, GID2, DID]),
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
    [<<"a00000002*groups*g00000001">>,
     <<"a00000002*groups*g00000002">>] =
	?rpc(exodm_db_account,list_groups, [2]),
    ok.

list_group_notifications(Cfg) ->
    [<<"https://ulf:wiger@localhost:8000/exodm/test_callback2">>,
     <<"https://ulf:wiger@localhost:8000/exodm/test_callback">>] =
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

store_exosense_yang(Cfg) ->
    ok = rscript(Cfg, store_exosense_yang_scr()).

store_exosense_yang_scr() ->
    codegen:exprs(
      fun() ->
	      exodm_db:transaction(
		fun(Db) ->
			exodm_db_session:set_auth_as_user(<<"ulf">>, Db),
			exodm_db_session:set_trusted_proc(),
			{ok, Bin} = file:read_file(
				      filename:join(code:priv_dir(ck3),
				      "yang/exosense.yang")),
			exodm_db_yang:write("system.exosense.yang", Bin),
			exodm_db_session:logout()
		end),
	      ok
      end).



store_exodm_yang(Cfg) ->
    ok = rscript(Cfg, store_exodm_yang_scr()).

store_exodm_yang_scr() ->
    codegen:exprs(
      fun() ->
	      exodm_db:transaction(
		fun(Db) ->
			exodm_db_session:set_auth_as_user(<<"ulf">>, Db),
			exodm_db_session:set_trusted_proc(),
			{ok, Bin} = file:read_file(
				      filename:join(code:priv_dir(exodm_db),
				      "yang/exodm.yang")),
			exodm_db_yang:write("system.exodm.yang", Bin),
			exodm_db_session:logout()
		end),
	      ok
      end).

store_config(Cfg) ->
    {ok, #conf_tree{root = R, tree = T}} =
	rscript(Cfg, store_config_scr()),
    R = <<"test">>,
    {T,T} = {T, [{<<"name">>, [], <<"test">>},
		 {<<"protocol">>, [], <<"exodm_bert">>},
		 {<<"values">>, [{<<"cksrv-address">>, [], <<"127.0.0.1">>},
				 {<<"kill-switch">>, [], 0},
				 {<<"wakeup-prof">>,
				  [{1, [{<<"data">>, [], <<"01010102">>},
					{<<"id">>, [], 1}]},
				   {2, [{<<"data">>, [], <<"01010103">>},
					{<<"id">>, [], 2}]}]}
				]},
		 {<<"yang">>, [], <<"ckp-cfg.yang">>}
		]},
    ok.

store_config_scr() ->
    codegen:exprs(
      fun() ->
	      exodm_db:transaction(
		fun(Db) ->
			exodm_db_session:set_auth_as_user(<<"ulf">>, Db),
			AID = exodm_db_session:get_aid(),
			{ok, <<"test">>} =
			    exodm_db_config:new_config_data(
			      AID,
			      <<"test">>, <<"ckp-cfg.yang">>,
			      <<"exodm_bert">>,
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
				}]}),
			exodm_db_config:read_config_data(AID, <<"test">>)
		end)
      end).

store_config2(Cfg) ->
    {ok, #conf_tree{root = R, tree = T}} =
	rscript(Cfg, store_config_scr2()),
    R = <<"test2">>,
    {T,T} = {T, [{<<"name">>, [], <<"test2">>},
		 {<<"protocol">>, [], <<"exodm_bert">>},
		 %% no 'values' entry
		 {<<"yang">>, [], <<"test.yang">>}
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
			    exodm_db_config:new_config_data(
			      AID,
			      <<"test2">>, <<"test.yang">>,
			      <<"exodm_bert">>,
			      {array, []}),
			exodm_db_config:read_config_data(AID, <<"test2">>)
		end)
      end).

add_config_data_member(Cfg) ->
    ok = rscript(Cfg, add_config_data_member_scr()),
    [<<"test">>, <<"test2">>] =
	?rpc(exodm_db_device, list_config_data, [<<"a00000002">>,
						 <<"x00000001">>]),
    ok.

add_config_data_member_scr() ->
    codegen:exprs(
      fun() ->
	      exodm_db:transaction(
		fun(Db) ->
			exodm_db_session:set_auth_as_user(<<"ulf">>, Db),
			AID = exodm_db_session:get_aid(),
			exodm_db_config:add_config_data_members(
			  AID, <<"test">>, [<<"x00000001">>]),
			exodm_db_config:add_config_data_members(
			  AID, <<"test2">>, [<<"x00000001">>])
		end)
      end).

%%% ==================================== Client BERT RPC Setup

start_rpc_client(Cfg) ->
    Auth = ?rpc(exodm_db_device, client_auth_config, [<<"a00000002">>,
						      <<"x00000001">>]),
    ?debugVal(Auth),
    Apps = [exo, bert, gproc, kvdb, exoport],
    [{A,ok} = {A, application:load(A)} || A <- Apps],
    [[application:set_env(A, K, V) || {K,V} <- L] ||
	{A, L} <- [{exoport, [{exodm_address, {"localhost", 9900}},
			      {bert_port, 9990}]} | Auth]],
    [{A,ok} = {A, application:start(A)} || A <- Apps],
    [{client_auth, Auth} | Cfg].

stop_rpc_client(_Cfg) ->
    Apps = [exoport, kvdb, bert, gproc, exo],
    [{A,ok} = {A, application:stop(A)} || A <- Apps],
    [{A,ok} = {A, application:unload(A)} || A <- Apps],
    ok.

%%% ==================================== Client BERT RPC Tests

client_ping(_Cfg) ->
       {reply, pong, []} = exoport:ping().


set_access(Cfg) ->
    load_this_module(Cfg),
    {ok, {Host, Port}} = application:get_env(exoport, exodm_address),
    {ok, Sn} = bert_rpc_exec:get_session(
		 Host, Port, [tcp], [{auto_connect,false}], 10000),
    A = gen_server:call(Sn, get_access),
    io:fwrite(user, "Old Access = ~p~n", [A]),
    ok = gen_server:call(
	   Sn, {set_access, [{redirect, [{{test,echo,1},
					  {?MODULE,test_echo,1}}]}]}),
    io:fwrite(user, "New Access = ~p~n", [gen_server:call(Sn, get_access)]),
    ok.

load_this_module(Cfg) ->
    {ok, Bin} = file:read_file(code:which(?MODULE)),
    {module, ?MODULE} =
	?rpc(code, load_binary,
	     [?MODULE, atom_to_list(?MODULE) ++ ".beam", Bin]),
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

%%% ==================================== End client BERT RPC


%%% ==================================== JSON-RPC Tests

start_http_client(_Cfg) ->
    application:start(crypto),
    application:start(public_key),
    ok = application:start(ssl),
    ok = lhttpc:start().

stop_http_client(_Cfg) ->
    ok = lhttpc:stop(),
    ok = application:stop(ssl),
    application:stop(public_key),
    application:stop(crypto).

json_rpc1(_Cfg) ->
    post_json_rpc({8000, "ulf", "wiger", "/exodm/rpc"},
		  "exodm:create-config-data", "1",
		  {struct,[{"name","test_cfg_1"},
			   {"yang", "exosense.yang"},
			   {"protocol","exodm_bert"},
			   {"values",{struct,[{"a", "1"},
					      {"b", "xx"}
					     ]}}
			  ]}).

device_json_rpc1(Cfg) ->
    ?loglevel(
       debug,
       begin
	   set_access(Cfg),
	   spawn_link(fun() ->
			      register(device_json_rpc1, self()),
			      receive
				  {Pid, got, Args} ->
				      Msg = proplists:get_value(message, Args,
								<<"huh?">>),
				      Reply = {notify, 'echo-callback',
					       [{message, Msg}]},
				      Pid ! {answer, Reply}
			      end
		      end),
	   post_json_rpc({8000, "ulf", "wiger", "/exodm/rpc"},
			 "test:echo", "2",
			 {struct, [{"device-id", "x00000001"},
				   {"message", "hello"}]})
       end).


post_json_rpc({Port, User, Pwd, Path}, Method, ID, Params) ->
    URL = lists:concat(["https://", User, ":", Pwd, "@localhost:",
			integer_to_list(Port), Path]),
    Body = json2:encode({struct, [{"json-rpc", "2.0"},
				  {"method", Method},
				  {"id", ID},
				  {"params", Params}]}),
    Res = lhttpc:request(URL, "POST",
			 [{"Content-Length", integer_to_list(
					       iolist_size(Body))},
			  {"Content-Type", "application/json-rpc"},
			  {"Host", "localhost"}],
			 Body, 3000),
    io:fwrite(user, "HTTP Res = ~p~n", [Res]),
    Res.

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
	   File = filename:join(D, "curpid.data"), list_to_binary(D)),
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
    in_dir(Top, fun() ->
			run("make release"),
			run("make generate")
		end).

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
    DevRun = filename:join(Top, "devrun"),
    MakeNode = filename:join(Top, "make_node"),
    Rel = filename:join(Top, "rel/exodm"),
    in_dir(Dir, fun() ->
			?debugVal(
			   os:cmd(MakeNode ++ " -target " ++ Dir
				  ++ " -rel " ++ Rel ++ " -- -name " ++ NodeStr)),
			?debugVal(os:cmd(Rel ++ "/bin/exodm start"))
		end),
    [{dir, Dir},
     {node, list_to_atom(NodeStr)}].

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
