-module(exodm_rebar_plugin).

-export(['post_create-node'/2]).


'post_create-node'({config,_,Env}, _File) ->
    case file:consult("reltool.config.src") of
	{ok, Terms} ->
	    New = lists:map(fun(T) -> expand(T, Env) end, Terms),
	    write_to_file("reltool.config", New),
	    ok;
	_ ->
	    ok
    end.


expand({sys, Params}, Env) ->
    Ps1 = [P || P <- Params,
		element(1,P) =/= app
	  ],
    Ps2 = replace_rels(Ps1, Env),
    AllApps = lists:usort(lists:concat([As || {rel,_,_,As} <- Ps2])),
    {sys, Ps2 ++ [{app, A, [{incl_cond, include}]} || A <- AllApps]};
expand(T, _) ->
    T.

replace_rels(Ts, Env) ->
    lists:map(fun({rel,R,V,_} = T) ->
		      case [As || {rel_apps,Rx,As} <- Env,
				  Rx == R] of
			  [] ->
			      T;
			  [As1] ->
			      {rel,R,V,As1}
		      end;
		 (T) ->
		      T
	      end, Ts).

write_to_file(F, Terms) ->
    {ok, Fd} = file:open(F, [write]),
    try
	[io:fwrite(Fd, "~p.~n~n", [T]) || T <- Terms]
    after
	file:close(Fd)
    end.


