-module(exodm_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("feuerlabs_eunit.hrl").

-include_lib("parse_trans/include/codegen.hrl").
-include_lib("kvdb/include/kvdb_conf.hrl").

-define(my_t(E), ?_t(?dbg(E))).
-define(rpc(M,F,A), rpc(Cfg, M, F, A)).

exodm_test_() ->
    {setup,
     fun() ->
	     {ok, CWD} = file:get_cwd(),
	     Top = filename:dirname(CWD),
	     make(Top),
	     Cfg = start_exodm(Top),
	     ok = await_started(Cfg),
	     Cfg
     end,
     fun(Config) ->
	     ?debugVal(stop_exodm(Config))
     end,
     fun(Config) -> [
		     ?my_t(populate(Config)),
		     ?my_t(list_accounts(Config)),
		     ?my_t(list_users(Config)),
		     ?my_t(store_yang(Config)),
		     ?my_t(store_config(Config)),
		     ?my_t(add_config_data_member(Config))
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
    {ok, GID} = ?rpc(exodm_db_group,new,
		     [
		      AID1, [{name, <<"feuerlabs">>},
			     {url, "http://flcallback:8080/exodm/callback"}]
		     ]),
    ok = ?rpc(exodm_db_device, new,
	      [AID2, DID = <<"x00000001">>,
	       [{'__ck',<<2,0,0,0,0,0,0,0>>},
		{'__sk',<<1,0,0,0,0,0,0,0>>},
		{msisdn,"070100000001"},
		{groups, [GID]}
	       ]]),
    ?debugFmt("AID1 = ~p; AID2 = ~p; GID = ~p; DID = ~p~n",
	      [AID1, AID2, GID, DID]),
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

store_yang(Cfg) ->
    ok = rscript(Cfg, store_yang_scr()).

store_yang_scr() ->
    codegen:exprs(
      fun() ->
	      exodm_db:transaction(
		fun(Db) ->
			exodm_db_session:set_auth_as_user(<<"ulf">>, Db),
			{ok, Bin} = file:read_file(
				      filename:join(code:priv_dir(ck3),
						    "yang/exosense.yang")),
			exodm_db_yang:write("exosense.yang", Bin)
		end),
	      ok
      end).

store_config(Cfg) ->
    {ok, #conf_tree{root = R, tree = T} = CT} =
	rscript(Cfg, store_config_scr()),
    R = <<"test">>,
    T = [{<<"name">>, [], <<"test">>},
	 {<<"values">>, [{<<"cksrv-address">>, [], <<"127.0.0.1">>},
			 {<<"kill-switch">>, [], 0},
			 {<<"wakeup-prof">>,
			  [{1, [{<<"data">>, [], <<"01010102">>},
				{<<"id">>, [], 1}]},
			   {2, [{<<"data">>, [], <<"01010103">>},
				{<<"id">>, [], 2}]}]}
			]},
	 {<<"yang">>, [], <<"ckp-cfg.yang">>}
	],
    ok.

store_config_scr() ->
    codegen:exprs(
      fun() ->
	      exodm_db:transaction(
		fun(Db) ->
			exodm_db_session:set_auth_as_user(<<"ulf">>, Db),
			{ok, <<"test">>} =
			    exodm_db_config:new_config_data(
			      <<"test">>, <<"ckp-cfg.yang">>,
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
			exodm_db_config:read_config_data(<<"test">>)
		end)
      end).

add_config_data_member(Cfg) ->
    ok = rscript(Cfg, add_config_data_member_scr()),
    [<<"test">>] = ?rpc(exodm_db_device, list_config_data, [<<"a00000002">>,
							    <<"x00000001">>]),
    ok.

add_config_data_member_scr() ->
    codegen:exprs(
      fun() ->
	      exodm_db:transaction(
		fun(Db) ->
			exodm_db_session:set_auth_as_user(<<"ulf">>, Db),
			exodm_db_config:add_config_data_members(
			  <<"test">>, [<<"x00000001">>])
		end)
      end).

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

make(Top) ->
    in_dir(Top, fun() ->
			os:cmd("make dev")
		end).

start_exodm(Top) ->
    net_kernel:start([exodm_test, longnames]),
    Dir = filename:absname("exodm_tmp"),
    NodeStr = "exodm_n1@" ++ hostname(),
    case filelib:is_dir(Dir) of
	true -> os:cmd("rm -r " ++ Dir);
	false -> ok
    end,
    ok = file:make_dir(Dir),
    DevRun = filename:join(Top, "devrun"),
    in_dir(Dir, fun() ->
			?debugVal(
			   os:cmd(DevRun ++ " -name " ++ NodeStr ++ " -detached"))
		end),
    [{dir, Dir},
     {node, list_to_atom(NodeStr)}].

stop_exodm(Cfg) ->
    Node = proplists:get_value(node, Cfg),
    {badrpc, _} = rpc:call(Node, erlang, halt, []),
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
