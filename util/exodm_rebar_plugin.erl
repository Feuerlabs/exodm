-module(exodm_rebar_plugin).

-export(['post_create-node'/2,
	 post_generate/2]).

'post_create-node'(Config, _File) when element(1,Config)==config ->
    %% Rebar really should export some accessor functions
    %% for the config record
    Env = element(3, Config),
    case file:consult("reltool.config.src") of
	{ok, Terms} ->
	    Vars = [{"TARGET", get_target(Env)},
		    {"VSN", get_vsn(Env)}],
	    New = lists:map(fun(T) -> expand(T, Env) end,
			    expand_env(Vars, Terms)),
	    write_to_file("reltool.config", New),
	    write_vm_args(),
	    ok;
	_ ->
	    ok
    end.

get_target(Env) ->
    case lists:keyfind(target, 1, Env) of
	{_, T} ->
	    lists:last(filename:split(T));
	false ->
	    io:fwrite("No target specified~n"
		      "Rebar globals = ~p~n", [application:get_all_env(rebar)]),
	    "exodm"
    end.

get_vsn(Env) ->
    case lists:keyfind(vsn, 1, Env) of
	{_, V} ->
	    V;
	false ->
	    io:fwrite("No version specified~n", []),
	    "1"
    end.

post_generate(Config, File) ->
    ReltoolConfig = load_config(File),
    TargetDir = get_target_dir(Config, ReltoolConfig),
    MakeNodeTgt = filename:join(TargetDir, "make_node"),
    {ok,_} = file:copy("../make_node", MakeNodeTgt),
    set_x_bit(MakeNodeTgt),
    CtlTarget = filename:join(TargetDir, "ctl"),
    {ok,_} = file:copy("../ctl", CtlTarget),
    set_x_bit(CtlTarget),
    {ok,_} = file:copy(File, filename:join(TargetDir, filename:basename(File))),
    make_erts_link(TargetDir),
    ok.

get_target_dir(Config, ReltoolConfig) ->
    try rebar_rel_utils:get_target_dir(Config, ReltoolConfig)
    catch
	error:undef ->
	    rebar_rel_utils:get_target_dir(ReltoolConfig)
    end.

make_erts_link(Dir) ->
    [Erts] = filelib:wildcard(filename:join(Dir, "erts-*")),
    {ok,OldCWD} = file:get_cwd(),
    file:set_cwd(Dir),
    case file:make_symlink(filename:basename(Erts), "erts") of
	ok -> ok;
	{error,eexist} -> ok
    end,
    file:set_cwd(OldCWD).

load_config(File) ->
    case rebar_config:consult_file(File) of
	{ok, Terms} ->
	    Terms;
	Other ->
	    error(Other)
    end.

set_x_bit(F) ->
    file:change_mode(F, 8#00744).

expand({sys, Params}, Env) ->
    Ps1 = [P || P <- Params,
		element(1,P) =/= app
	  ],
    Ps2 = replace_rels(Ps1, Env),
    AllApps = lists:usort(
		  [app_name(A) ||
		      A <- lists:concat(
			     [As || {rel,_,_,As} <- Ps2])]),
    {sys, fix_lib_dirs(AllApps,
		       Ps2 ++ [{app, A, [{incl_cond, include}]} ||
				  A <- AllApps])};
expand(T, _) ->
    T.

app_name(A) when is_tuple(A) ->
    element(1, A);
app_name(A) when is_atom(A) ->
    A.

fix_lib_dirs(Apps, Ps) ->
    LibDirs = [normalize_fname(filename:absname(D)) ||
		  D <- proplists:get_value(lib_dirs, Ps, []),
		  is_legal_dir(D)],
    AppNames = [app_name(A) || A <- Apps],
    OldPath = code:get_path(),
    code:set_path(
      lists:concat([filelib:wildcard(
		      filename:join(D,"*/ebin")) || D <- LibDirs]) ++ OldPath),
    OTPLibD = otp_libdir(),
    ActualLibs = lists:foldl(fun(A, Acc) ->
				     case code:lib_dir(A) of
					 {error, _} ->
					     error({cannot_find_app, A});
					 D ->
					     store(filename:dirname(D), OTPLibD, Acc)
				     end
			     end, LibDirs, AppNames),
    lists:keystore(lib_dirs, 1, Ps, {lib_dirs, ActualLibs}).

normalize_fname(F) ->
    filename:join(normalize_fname_(filename:split(F))).

normalize_fname_([D, ".."|T]) ->
    normalize_fname_(T);
normalize_fname_([H|T]) ->
    [H|normalize_fname_(T)];
normalize_fname_([]) ->
    [].



is_legal_dir(D) ->
    case file:list_dir(D) of
	{error, _} ->
	    io:fwrite("NOTE: Dir can't be read: ~s~n", [D]),
	    false;
	{ok,_} ->
	    true
    end.

store(X, X, L) ->
    L;
store(X, _, L) ->
    case lists:member(X, L) of
	true ->
	    L;
	false ->
	    L ++ [X]
    end.

otp_libdir() ->
    filename:dirname(code:lib_dir(stdlib)).

replace_rels(Ts, Env) ->
    lists:flatmap(
      fun({rel,R,V,_} = T) ->
	      case [As || {rel_apps,Rx,As} <- Env,
			  Rx == R] of
		  [] ->
		      [T];
		  [As1] ->
		      [{rel,R,V,As1},
		       {rel,R++"_setup",V,setup_conv(As1)}]
	      end;
	 (T) ->
	      [T]
      end, Ts).


setup_conv(As) ->
    lists:map(
      fun(setup) -> setup;
	 ({setup,load}) -> setup;
	 (A) when A==kernel; A==stdlib; A==sasl ->
	      A;
	 (A) when is_atom(A) ->
	      {A, load};
	 ({A,Is}) when is_list(Is) ->
	      {A, load, Is};
	 ({A,T}) when is_atom(T) ->
	      {A, load};
	 ({A,T,Is}) when is_atom(T), is_list(Is) ->
	      {A, load, Is}
      end, As).

write_to_file(F, Terms) ->
    {ok, Fd} = file:open(F, [write]),
    try
	[io:fwrite(Fd, "~p.~n~n", [T]) || T <- Terms]
    after
	file:close(Fd)
    end.

write_vm_args() ->
    case file:copy("../priv/templates/simplenode.vm.args",
		   "files/vm.args") of
	{ok, _BytesCopied} ->
	    ok;
	Other ->
	    error({could_not_copy_vm_args, Other})
    end.

%% Copied from http://github.com/uwiger/setup
%%
expand_env(Vs, T) when is_tuple(T) ->
    list_to_tuple([expand_env(Vs, X) || X <- tuple_to_list(T)]);
expand_env(Vs, L) when is_list(L) ->
    case is_string(L) of
        true ->
            do_expand_env(L, Vs, list);
	false ->
	    [expand_env(Vs, X) || X <- L]
    end;
expand_env(Vs, B) when is_binary(B) ->
    do_expand_env(B, Vs, binary);
expand_env(_, X) ->
    X.

do_expand_env(X, Vs, Type) ->
    lists:foldl(fun({K, Val}, Xx) ->
			re:replace(Xx, [$\\, $$ | K], Val, [{return,Type}])
		end, X, Vs).

is_string(S) ->
    lists:all(fun(C) when 0 < C, C =< 255 -> true;
		 (_) -> false
	      end, S).
