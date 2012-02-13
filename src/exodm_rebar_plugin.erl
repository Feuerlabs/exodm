-module(exodm_rebar_plugin).

-export(['post_create-node'/2,
	 post_generate/2]).


'post_create-node'({config,_,Env}, _File) ->
    case file:consult("reltool.config.src") of
	{ok, Terms} ->
	    New = lists:map(fun(T) -> expand(T, Env) end, Terms),
	    write_to_file("reltool.config", New),
	    ok;
	_ ->
	    ok
    end.

post_generate(Config, File) ->
    ok.

expand({sys, Params}, Env) ->
    Ps1 = [P || P <- Params,
		element(1,P) =/= app
	  ],
    Ps2 = replace_rels(Ps1, Env),
    AllApps = lists:usort(lists:concat([As || {rel,_,_,As} <- Ps2])),
    {sys, fix_lib_dirs(AllApps,
		       Ps2 ++ [{app, app_name(A), [{incl_cond, include}]} || A <- AllApps])};
expand(T, _) ->
    T.

app_name(A) when is_tuple(A) ->
    element(1, A);
app_name(A) when is_atom(A) ->
    A.

fix_lib_dirs(Apps, Ps) ->
    LibDirs = [filename:absname(D) || D <- proplists:get_value(lib_dirs, Ps, []),
				      is_legal_dir(D)],
    io:fwrite("LibDirs = ~p~n", [LibDirs]),
    AppNames = [app_name(A) || A <- Apps],
    OldPath = code:get_path(),
    code:set_path(lists:concat([filelib:wildcard(
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
    io:fwrite("ActualLibs = ~p~n", [ActualLibs]),
    lists:keystore(lib_dirs, 1, Ps, {lib_dirs, ActualLibs}).

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
    lists:flatmap(fun({rel,R,V,_} = T) ->
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

find(A, [A|_]) -> {ok, A};
find(A, [H|_]) when element(1,H) == A -> {ok, H};
find(A, [_|T]) ->
    find(A, T);
find(_, []) ->
    error.

write_to_file(F, Terms) ->
    {ok, Fd} = file:open(F, [write]),
    try
	[io:fwrite(Fd, "~p.~n~n", [T]) || T <- Terms]
    after
	file:close(Fd)
    end.


