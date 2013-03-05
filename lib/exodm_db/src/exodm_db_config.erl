%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     General config model
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_config).

-export([init/1]).
-export(
   [new_config_set/2,              %% (AID, Options)
    update_config_set/3,           %% (AID, Name, Options)
    read_config_set/2,             %% (AID, Name)
    read_config_set_values/2,      %% (AID, Name)
    get_yang_spec/2,               %% (AID, Name)
    get_url/2,                     %% (AID, Name)
    delete_config_set/2,           %% (AID, Name)
    create_yang_module/4,          %% (AID, <<"user">>|<<"system">>, File, Yang)
    add_members_to_config_sets/3,
    add_config_set_members/3,      %% (AID, CfgDataName, [DeviceID])
    remove_members_from_config_sets/3,
    delete_config_set_members/3,   %% (AID, Name, DIDs)
    device_is_deleted/3,           %% (AID, DID, ConfigSets)
    list_config_set_members/1,     %% (Name) -> (AID, Name)
    list_config_set_members/2,     %% (AID, CfgDataName)
    cache_values/2,                %% (AID, Name)
    map_device_to_cached_values/4, %% (AID, Name)
    get_cached/4,
    switch_to_active/2             %% (AID, Name)
   ]).
-export([table/1]).

-import(exodm_db, [binary_opt/2]).

-include_lib("kvdb/include/kvdb_conf.hrl").
-include_lib("lager/include/log.hrl").

%%
%% /u<UID>/devices/x<DID>/config/<target>/<tree>
%%

init(AID) ->
    exodm_db:in_transaction(
      fun(_) ->
	      kvdb_conf:add_table(table(AID), [{encoding,{raw,sext,term}}]),
	      kvdb_conf:add_table(cache(AID), [{encoding,{raw,sext,term}}])
      end).

table(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    <<AID/binary, "_config">>.

cache(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    <<AID/binary, "_conf_cache">>.

new_config_set(AID, Opts) ->
    Tab = table(AID),
    Name = binary_opt(name, Opts),
    Yang = binary_opt(yang, Opts),
    URL = binary_opt('notification-url', Opts),
    NameKey = exodm_db:encode_id(Name),
    Values = proplists:get_value(values, Opts, []),
    exodm_db:transaction(
      fun(_) ->
	      case exist_(Tab, NameKey) of
		  true ->
		      error(exists);
		  false ->
		      validate_config(Tab, AID, Yang, Values),
		      write(Tab, NameKey, <<"name">>, Name),
		      write(Tab, NameKey, <<"yang">>, Yang),
		      write(Tab, NameKey, <<"url">>, URL),
		      ValsToWrite = value_tree(NameKey, Values),
		      %% write_values(Tab, NameKey, Values),
		      [kvdb_conf:write(Tab, Obj) || Obj <- ValsToWrite],
		      {ok, Name}
	      end
      end).

update_config_set(AID, Name0, Opts) ->
    Tab = table(AID),
    Name = to_binary(Name0),
    NameKey = exodm_db:encode_id(Name),
    case get_yang_spec(AID, NameKey) of
        {ok, _YangSpec} ->
            exodm_db:transaction(
              fun(_) ->
                      case exist_(Tab, NameKey) of
                          true ->
			      CT = kvdb_conf:read_tree(Tab, NameKey),
%                              validate_config(
%%				Tab, AID, YangSpec, Values),
			      if_opt('notification-url', Opts,
				     fun(URL) ->
					     write(Tab, NameKey,
						   <<"url">>, to_binary(URL))
				     end),
			      case lists:keyfind(yang, 1, Opts) of
				  {_, _Yang} ->
				      error(cannot_update_yang_spec);
				  false -> ok
			      end,
			      if_opt(values, Opts,
				     fun(Values) ->
					     ValsToWrite =
						 update_value_tree(Values, CT),
					     [kvdb_conf:write(Tab, Obj) ||
						 Obj <- ValsToWrite]
				     end),
                              {ok, Name};

                          false ->
                              { error, not_found }
                      end
              end);
        E ->
            { error, E }
    end.

if_opt(K, Opts, F) ->
    case lists:keyfind(K, 1, Opts) of
	{_, V} -> F(V);
	false  -> ok
    end.

read_config_set(AID, Name) ->
    Tab = table(AID),
    NameKey = exodm_db:encode_id(Name),
    exodm_db:transaction(
      fun(_) ->
	      case exist_(Tab, NameKey) of
		  true ->
		      [{name, exodm_db:decode_id(NameKey)}]
			  ++ read(Tab, NameKey, <<"yang">>, yang)
			  ++ read(Tab, NameKey, <<"url">>, 'notification-url');
		  false ->
		      {error, not_found}
	      end
      end).

get_yang_spec(AID, Name) ->
    Tab = table(AID),
    NameKey = exodm_db:encode_id(Name),
    case exodm_db:read(Tab, exodm_db:join_key(NameKey, <<"yang">>)) of
	{ok, {_, _, Y}} ->
	    {ok, Y};
	{error, _} = E ->
	    E
    end.

get_url(AID, Name) ->
    Tab = table(AID),
    NameKey = exodm_db:encode_id(Name),
    case exodm_db:read(Tab, exodm_db:join_key(NameKey, <<"url">>)) of
	{ok, {_, _, U}} ->
	    {ok, U};
	{error, _} = E ->
	    E
    end.

read_config_set_values(AID, Name) ->
    Tab = table(AID),
    NameKey = exodm_db:join_key(exodm_db:encode_id(Name), <<"values">>),
    exodm_db:transaction(
      fun(_) ->
	      case kvdb_conf:read_tree(Tab, NameKey) of
		  #conf_tree{} = CT ->
		      {ok, CT};
		  [] ->
		      {error, not_found}
	      end
      end).

cache_values(AID0, Name0) ->
    ?debug("cache_values(~p, ~p)~n", [AID0, Name0]),
    AID = exodm_db:account_id_key(AID0),
    Name = exodm_db:encode_id(Name0),
    Cache = cache(AID),
    exodm_db:in_transaction(
      fun(_Db) ->
	      case read_config_set_values(AID, Name) of
		  {ok, CT} ->
		      ?debug("read CT = ~p~n", [CT]),
		      Key = exodm_db:join_key(Name, <<"values">>),
		      {ok, Last} = kvdb_conf:last_list_pos(Cache, Key),
		      ?debug("Last = ~p~n", [Last]),
		      Ref = Last+1,
		      exodm_db:write(Cache, exodm_db:list_key(Key, Ref), CT),
		      {ok, Ref};
		  {error, not_found} = Err ->
		      Err
	      end
      end).

switch_to_active(AID0, Name0) ->
    ?debug("switch_to_active(~p, ~p)~n", [AID0, Name0]),
    AID = exodm_db:account_id_key(AID0),
    Name = exodm_db:encode_id(Name0),
    Tab = table(AID),
    exodm_db:in_transaction(
      fun(_Db) ->
	      case read_config_set_values(AID, Name) of
		  {ok, CT} ->
		      kvdb_conf:delete_tree(
			Tab, exodm_db:join_key(Name, <<"values">>)),
		      Root = exodm_db:join_key(Name, <<"active">>),
		      kvdb_conf:write_tree(Tab, Root, CT);
		  {error, not_found} = Err ->
		      error(Err)
	      end
      end).

map_device_to_cached_values(AID0, Name0, Ref, DID) ->
    AID = exodm_db:account_id_key(AID0),
    Name = exodm_db:encode_id(Name0),
    Cache = cache(AID),
    exodm_db:in_transaction(
      fun(_Db) ->
	      Key = exodm_db:join_key(Name, <<"values">>),
	      RefKey = exodm_db:list_key(Key, Ref),
	      case kvdb_conf:read(Cache, RefKey) of
		  {ok, _} ->
		      DIDKey = exodm_db:join_key(
				 [RefKey, <<"did">>, DID]),
		      exodm_db:write(Cache, DIDKey, <<>>),
		      ok;
		  {error, _} ->
		      error(illegal_map_to_cache)
	      end
      end).

get_cached(AID0, Name0, Ref, DID) ->
    ?debug("get_cached(~p, ~p, ~p, ~p)~n", [AID0,Name0,Ref,DID]),
    AID = exodm_db:account_id_key(AID0),
    Name = exodm_db:encode_id(Name0),
    Cache = cache(AID),
    exodm_db:in_transaction(
      fun(_Db) ->
	      Key = exodm_db:join_key(Name, <<"values">>),
	      RefKey = exodm_db:list_key(Key, Ref),
	      case kvdb_conf:read(Cache, RefKey) of
		  {ok, {_, _, #conf_tree{} = CT}} ->
		      ?debug("get_cached: CT = ~p~n", [CT]),
		      DIDKey = exodm_db:join_key(
				 [RefKey, <<"did">>, DID]),
		      kvdb_conf:delete(Cache, DIDKey),
		      case kvdb_conf:first_child(
			     Cache, exodm_db:join_key(Key, <<"did">>)) of
			  {ok, _} -> ok;
			  done ->
			      kvdb_conf:delete_tree(Cache, RefKey)
		      end,
		      ?debug("Fetched CT = ~p~n", [CT]),
		      Root = kvdb_conf:unescape_key(Key),
		      {ok, CT#conf_tree{root = Root}};
		  {error, _} = Error ->
		      Error
	      end
      end).



delete_config_set(AID, Name) ->
    Tab = table(AID),
    NameKey = exodm_db:encode_id(to_binary(Name)),
    exodm_db:transaction(
      fun(_) ->
	      kvdb_conf:delete_all(Tab, NameKey)
      end).

create_yang_module(AID, Repository0, File, Yang0) ->
    check_access(AID),
    Repository = to_binary(Repository0),
    Yang = to_binary(Yang0),
    case Repository of
	<<"user">> ->
	    %% This we can easily support
	    exodm_db:in_transaction(
	      fun(_) ->
		      exodm_db_yang:write(to_binary(File), Yang)
	      end);
	<<"system">> ->
	    %% Need a system access model before we can allow this. FIXME
	    error(unauthorized)
    end.

add_members_to_config_sets(AID0, Names, DIDs) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:in_transaction(
      fun(_) ->
	      lists:foreach(
		fun(Name) ->
			add_config_set_members(AID, Name, DIDs)
		end, Names)
      end).

add_config_set_members(AID0, Name0, DIDs) ->
    AID = exodm_db:account_id_key(AID0),
    Tab = table(AID),
    Name = exodm_db:encode_id(Name0),
    Key = exodm_db:join_key(Name, <<"members">>),
    exodm_db:in_transaction(
      fun(_) ->
	      case exist_(Tab, Name) of
		  true ->
		      lists:foreach(
			fun(DID0) ->
				DID = exodm_db:encode_id(DID0),
				case exodm_db_device:exist(AID, DID) of
				    true ->
					write(Tab, Key, DID, <<>>),
					exodm_db_device:add_config_set(
					  AID, DID, Name);
				    false ->
					error({unknown_device, [AID,DID]})
				end
			end, DIDs);
		  false ->
		      error({unknown_config_set, Name})
	      end
      end).

remove_members_from_config_sets(AID0, Names, DIDs) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:in_transaction(
      fun(_) ->
	      lists:foreach(
		fun(Name) ->
			delete_config_set_members(AID, Name, DIDs)
		end, Names)
      end).

delete_config_set_members(AID0, Name0, DIDs) ->
    AID = exodm_db:account_id_key(AID0),
    Tab = table(AID),
    Name = exodm_db:encode_id(Name0),
    Key = exodm_db:join_key(Name, <<"members">>),
    exodm_db:in_transaction(
      fun(_) ->
	      case exist_(Tab, Name) of
		  true ->
		      lists:foreach(
			fun(DID0) ->
				DID = exodm_db:encode_id(DID0),
				delete(Tab, Key, DID),
				exodm_db_device:remove_config_set(
				  AID, DID, Name)
			end, DIDs);
		  false ->
		      ok
	      end
      end).

device_is_deleted(AID0, DID0, Configs) ->
    AID = exodm_db:account_id_key(AID0),
    Tab = table(AID),
    DID = exodm_db:encode_id(DID0),
    exodm_db:in_transaction(
      fun(_) ->
	      lists:foreach(
		fun(Name0) ->
			Name = exodm_db:encode_id(Name0),
			Key = exodm_db:join_key(Name, <<"members">>),
			delete(Tab, Key, DID)
		end, Configs)
      end).


list_config_set_members(Name) ->
    AID = exodm_db_session:get_aid(),
    list_config_set_members(AID, Name).

list_config_set_members(AID, Name0) ->
    Tab = table(AID),
    Name = to_binary(Name0),
    Key = exodm_db:join_key(exodm_db:encode_id(Name), <<"members">>),
    exodm_db:in_transaction(
      fun(_) ->
	      lists:reverse(
		kvdb_conf:fold_children(
		  Tab,
		  fun(K, Acc) ->
			  [lists:last(exodm_db:split_key(K))|Acc]
		  end, [], Key))
      end).

exist(AID, Name) ->
    exodm_db:in_transaction(
      fun(_) ->
	      exist_(table(AID), Name)
      end).

exist_(Tab, Name) ->
    Key = exodm_db:join_key([exodm_db:encode_id(Name), <<"name">>]),
    case exodm_db:read(Tab, Key) of
	{error, not_found} ->
	    false;
	{ok, _} ->
	    true
    end.

validate_config(_Tab, _AID, _Yang, _Values) ->
    %% We don't yet do any validation. FIXME
    ok.

write(Tab, Name, AKey, Value) ->
    Key = exodm_db:join_key([Name, to_binary(AKey)]),
    write(Tab, Key, Value).

write(Tab, Key, Value) ->
    exodm_db:write(Tab, Key, Value).

%% This doesn't actually produce a tree, but a *flattened* tree consisting
%% only of the objects under <<"values">>.
value_tree(_, []) ->
    [];
value_tree(Root, Values) ->
    kvdb_conf:flatten_tree(
      #conf_tree{root = Root,
		 tree = [{<<"values">>, to_tree_(Values)}]}).

update_value_tree(Values, #conf_tree{root = Root, tree = T}) ->
    ?debug("update_value_tree(~p, T = ~p)~n", [Values, T]),
    Vs = case lists:keyfind(<<"values">>, 1, T) of
	     {_, X} -> X;
	     false -> []
	 end,
    NewVs = update_tree_(Values, Vs),
    ?debug("NewVs = ~p ~n", [NewVs]),
    kvdb_conf:flatten_tree(#conf_tree{root = Root,
				      tree = [{<<"values">>, NewVs}]}).
    %% T1 = lists:keyreplace(<<"values">>, 1, T, {<<"values">>, NewVs}),
    %% Tree#conf_tree{tree = T1}.

update_tree_({struct, Elems}, Tree) ->
    lists:foldl(
      fun({K0,V}, Acc) ->
	      K = to_binary(K0),
	      case lists:keyfind(K, 1, Acc) of
		  {_, SubT} ->
		      lists:keyreplace(
			K, 1, Acc, update_node(K, V, SubT));
		  {_, [], _} ->
		      lists:keyreplace(K, 1, Acc, update_node(K, V, []));
		  false ->
		      ordered_insert(K, V, Acc)
	      end
      end, Tree, Elems);
update_tree_({array, Elems} = A, Tree) ->
    case lists:all(fun(X) -> is_integer(element(1,X)) end, Tree) of
	true ->
	    update_array(Elems, Tree);
	false ->
	    to_tree_(A)
    end;
update_tree_({K0,V}, Tree) ->
    K = to_binary(K0),
    case is_array(Tree) of
	true ->
	    [{K, [], V}];
	false ->
	    case lists:keyfind(K, 1, Tree) of
		false ->
		    ordered_insert(K, V, Tree);
		{_, SubT} ->
		    lists:keyreplace(K, 1, Tree, update_node(K, V, SubT));
		{_, _, _} ->
		    lists:keyreplace(K, 1, Tree, update_node(K, V, []))
	    end
    end.

update_array([{struct, [{K0,V}]}|Elems], Array) ->
    K = to_binary(K0),
    update_array(Elems, keystore_array(K, V, Array));
update_array([{struct,_} = S|Elems], Array) ->
    update_array(Elems, append_to_array(Array, S));
update_array([{array,_} = A|Elems], Array) ->
    update_array(Elems, append_to_array(Array, A));
update_array([{K0,V}|Elems], Array) ->
    K = to_binary(K0),
    update_array(Elems, keystore_array(K, V, Array));
update_array([X|Elems], Array) ->
    update_array(Elems, append_to_array(Array, X));
update_array([], Array) ->
    Array.

append_to_array(A, X) -> A ++ [{length(A)+1, to_tree_(X)}].

keystore_array(K, V, [{P, [{K,_,_}]}|A]) ->
    [{P, [update_node(K, V, [])]}|A];
keystore_array(K, V, [Last]) ->
    P = element(1, Last),
    [Last, {P+1, [update_node(K, V, [])]}];
keystore_array(K, V, [H|T]) ->
    [H | keystore_array(K, V, T)];
keystore_array(K, V, []) ->
    [{1, [update_node(K, V, [])]}].

is_array(Tree) ->
    lists:all(fun(X) -> is_integer(element(1,X)) end, Tree).

update_node(K, {struct,_} = S, T) ->
    {K, update_tree_(S, T)};
update_node(K, {array,_} = A, T) ->
    {K, update_tree_(A, T)};
update_node(K, V, _) ->
    {K, [], V}.

ordered_insert(K, V, [H|T]) when element(1,H) < K ->
    [H|ordered_insert(K, V, T)];
ordered_insert(K, V, T) ->
    H = case V of
	    {struct,_} -> {K, to_tree_(V)};
	    {array,_}  -> {K, to_tree_(V)};
	    _ ->
		{K, [], V}
	end,
    [H|T].



to_tree_({struct, Elems}) ->
    lists:map(
       fun({T,_} = X) when T==array; T==struct ->
 	      to_tree_(X);
 	 ({K,V}) ->
	       to_node_({K,V})
       end, Elems);
to_tree_({array, Elems}) ->
    {SubTree, _} = lists:mapfoldl(
 		     fun({T,_} = X, P) when T==array; T==struct ->
 			     {{P, to_tree_(X)}, P+1};
 			({K,V}, P) ->
			     case V of
				 {T1,_} when T1==array; T1==struct ->
				     {{P, [{K, to_tree_(V)}]}, P+1};
				 _ ->
				     {{P, [to_node_({K,V})]}, P+1}
			     end;
			(X, P) ->
			     {{P, [], X}, P+1}
 		     end, 1, Elems),
    SubTree;
to_tree_({_,_} = X) ->
    [to_node_(X)].

to_node_({K0,V}) ->
    K = to_binary(K0),
    case V of
	{T,_} when T==array; T==struct ->
	    {K, to_tree_(V)};
	_ ->
	    {K, [], V}
    end.



%% write_values(Tab, Name, Values) ->
%%     Key = exodm_db:join_key([Name, <<"values">>]),
%%     write_values_(Tab, Key, Values).

%% write_values_(Tab, Key, {struct, Elems}) ->
%%     lists:foreach(
%%       fun({K,{array,_} = A}) ->
%% 	      Key1 = exodm_db:join_key(Key, to_id(K)),
%% 	      write_values_(Tab, Key1, A);
%% 	 ({K, {struct,_} = S}) ->
%% 	      Key1 = exodm_db:join_key(Key, to_id(K)),
%% 	      write_values_(Tab, Key1, S);
%% 	 ({K,V}) ->
%% 	      Key1 = exodm_db:join_key(Key, to_id(K)),
%% 	      write(Tab, Key1, V)
%%       end, Elems);
%% write_values_(Tab, Key, {array, Elems}) ->
%%     lists:foldl(
%%       fun({array,__} = A, I) ->
%% 	      Key1 = exodm_db:list_key(Key, I),
%% 	      write_values_(Tab, Key1, A),
%% 	      I+1;
%% 	 ({struct,_} = S, I) ->
%% 	      Key1 = exodm_db:list_key(Key, I),
%% 	      write_values_(Tab, Key1, S),
%% 	      I+1;
%% 	 ({K, V}, I) ->
%% 	      Key1 = exodm_db:join_key(
%% 		       exodm_db:list_key(Key, I), to_id(K)),
%% 	      write(Tab, Key1, V),
%% 	      I+1
%%       end, 1, Elems).


%% tree_to_values({Key, As, <<>>, L}) when is_list(L) ->
%%     {decode_id(Key), As, tree_to_values(L)};
%% tree_to_values({Key, As, V}) ->
%%     Dec = decode_id(Key),
%%     {Dec, As, V};
%% tree_to_values([H|T]) ->
%%     Key = element(1, H),
%%     case is_list_key(exodm_db:decode_id(Key)) of
%% 	{true, Base, I1} ->
%% 	    {L1, L2} = pick_list(T, Base, [setelement(1, H, I1)]),
%% 	    [{Base, [], [tree_to_values(X) || X <- L1]} |
%% 	     [tree_to_values(X) || X <- L2]];
%% 	false ->
%% 	    [tree_to_values(H)|tree_to_values(T)]
%%     end;
%% tree_to_values([]) ->
%%     [].


%% pick_list([H|T], Base, Acc) ->
%%     K = exodm_db:decode_id(element(1,H)),
%%     case is_list_key(K) of
%% 	{true, Base, I} ->
%% 	    pick_list(T, Base, [setelement(1, H, I)|Acc]);
%% 	false ->
%% 	    {lists:reverse(Acc), T}
%%     end;
%% pick_list([], _, Acc) ->
%%     {lists:reverse(Acc), []}.

%% decode_id(I) when is_integer(I) ->
%%     I;
%% decode_id(B) when is_binary(B) ->
%%     exodm_db:decode_id(B).

%% is_list_key(I) when is_integer(I) -> false;
%% is_list_key(K) ->
%%     case re:run(K, <<"\\[">>, [global]) of
%% 	{match, Ps} ->
%% 	    {P,_} = lists:last(lists:flatten(Ps)),
%% 	    Sz = byte_size(K),
%% 	    N = Sz - P -2,
%% 	    case K of
%% 		<<Base:P/binary, "[", Ib:N/binary, "]">> ->
%% 		    {true, Base, exodm_db:id_key_to_integer(Ib)};
%% 		_ ->
%% 		    false
%% 	    end;
%% 	_ ->
%% 	    false
%%     end.

to_binary(X) when is_atom(X) -> atom_to_binary(X, latin1);
to_binary(X) when is_binary(X) -> X;
to_binary(X) when is_list(X) -> list_to_binary(X).

%% to_id(X) ->
%%     exodm_db:encode_id(to_binary(X)).

check_access(AID) ->
    AID = exodm_db_session:get_aid().

read(Tab, Key, SubKey, AttrName) ->
    case exodm_db:read(Tab, exodm_db:join_key(Key, SubKey)) of
	{ok, {_, _, Val}} ->
	    [{AttrName, Val}];
	{error, not_found} ->
	    []
    end.

delete(Tab, Key, Sub) ->
    kvdb_conf:delete(Tab, kvdb_conf:join_key(Key, Sub)).
