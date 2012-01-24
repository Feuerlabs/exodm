-module(exodm_rebar_plugin).

-export(['post_create-node'/2,
	 post_generate/2]).


'post_create-node'({config,_,Env}, _File) ->
    io:fwrite("~p:'post_create-node'(...)~n", [?MODULE]),
    case file:consult("reltool.config.src") of
	{ok, Terms} ->
	    New = lists:map(fun(T) -> expand(T, Env) end, Terms),
	    write_to_file("reltool.config", New),
	    ok;
	_ ->
	    ok
    end.

post_generate(Config, File) ->
    io:fwrite("post_generate()~n"
	      "Config = ~p.~n"
	      "File = ~p.~n", [Config, File]),
    ok.

expand({sys, Params}, Env) ->
    Ps1 = [P || P <- Params,
		element(1,P) =/= app
	  ],
    Ps2 = replace_rels(Ps1, Env),
    AllApps = lists:usort(lists:concat([As || {rel,_,_,As} <- Ps2])),
    {sys, Ps2 ++ [{app, app_name(A), [{incl_cond, include}]} || A <- AllApps]};
expand(T, _) ->
    T.

app_name(A) when is_tuple(A) ->
    element(1, A);
app_name(A) when is_atom(A) ->
    A.


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


