%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     General config model
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_config).

-export([init/1]).
-export(
   [new_config_data/5,         %% (AID, Name, Yang, Protocol, Values)
    update_config_data/3,      %% (AID, Name0, Values)
    read_config_data/2,        %% (AID, Name)
    read_config_data_values/2, %% (AID, Name)
    get_yang_spec/2,           %% (AID, Name)
    get_protocol/2,            %% (AID, Name) -> (AID, Name, <<"exodm_bert">>)
    get_protocol/3,            %% (AID, Name, Default)
    delete_config_data/2,      %% (AID, Name)
    create_yang_module/4,      %% (AID, <<"user">>|<<"system">>, File, Yang)
    add_config_data_members/3, %% (AID, CfgDataName, [DeviceID])
    list_config_data_members/2,%% (AID, CfgDataName)
    list_config_data_members/1 %% (Name)
   ]).
-export([table/1]).

-include_lib("kvdb/include/kvdb_conf.hrl").

%%
%% /u<UID>/devices/x<DID>/config/<target>/<tree>
%%

init(AID) ->
    exodm_db:in_transaction(
      fun(_) ->
	      kvdb_conf:add_table(table(AID), [{encoding,{raw,sext,term}}])
      end).

table(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    <<AID/binary, "_config">>.

new_config_data(AID, Name0, Yang0, Protocol, Values) ->
    Tab = table(AID),
    Name = to_binary(Name0),
    Yang = to_binary(Yang0),
    NameKey = exodm_db:encode_id(Name),
    exodm_db:transaction(
      fun(_) ->
	      case exists(Tab, NameKey) of
		  true ->
		      error(exists);
		  false ->
		      validate_config(Tab, AID, Yang, Protocol, Values),
		      write(Tab, NameKey, <<"name">>, Name),
		      write(Tab, NameKey, <<"yang">>, Yang),
		      write(Tab, NameKey, <<"protocol">>, Protocol),
		      ValsToWrite = value_tree(NameKey, Values),
		      %% write_values(Tab, NameKey, Values),
		      [kvdb_conf:write(Tab, Obj) || Obj <- ValsToWrite],
		      {ok, Name}
	      end
      end).

%% FIXME: If you add a key/val pair that was not present in the
%%        new_config_data() call, the entire config entry gets corrupted.
%%
update_config_data(AID, Name0, Values) ->
    Tab = table(AID),
    Name = to_binary(Name0),
    NameKey = exodm_db:encode_id(Name),
    Protocol = get_protocol(AID, Name0),
    case get_yang_spec(AID, NameKey) of
        {ok, YangSpec} ->
            exodm_db:transaction(
              fun(_) ->
                      case exists(Tab, NameKey) of
                          true ->
			      CT = kvdb_conf:read_tree(Tab, NameKey),
                              validate_config(
				Tab, AID, YangSpec, Protocol, Values),
			      ValsToWrite =
				  update_value_tree(Values, CT),
			      [kvdb_conf:write(Tab, Obj) || Obj <- ValsToWrite],
                              {ok, Name};

                          false ->
                              { error, not_found }
                      end
              end);
        E ->
            { error, E }
    end.

read_config_data(AID, Name) ->
    Tab = table(AID),
    NameKey = exodm_db:encode_id(Name),
    exodm_db:transaction(
      fun(_) ->
	      case exists(Tab, NameKey) of
		  true ->
		     case kvdb_conf:read_tree(Tab, NameKey) of
			 #conf_tree{} = CT ->
			     {ok, CT};
			 Other ->
			     error({unexpected, Other})
		     end;
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

get_protocol(AID, Name) ->
    get_protocol(AID, Name, <<"exodm_bert">>).

get_protocol(AID, Name, Default) ->
    Tab = table(AID),
    NameKey = exodm_db:encode_id(Name),
    case exodm_db:read(Tab, exodm_db:join_key(NameKey, <<"protocol">>)) of
	{ok, {_, _, P}} ->
	    P;
	{error, _} ->
	    Default
    end.

read_config_data_values(AID, Name) ->
    Tab = table(AID),
    NameKey = exodm_db:join_key(exodm_db:encode_id(Name), <<"values">>),
    exodm_db:transaction(
      fun(_) ->
	      case exists(Tab, NameKey) of
		  true ->
		     case kvdb_conf:read_tree(Tab, NameKey) of
			 #conf_tree{} = CT ->
			     {ok, CT};
			 Other ->
			     error({unexpected, Other})
		     end;
		  false ->
		      {error, not_found}
	      end
      end).


delete_config_data(AID, Name) ->
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

add_config_data_members(AID0, Name0, DIDs) ->
    AID = exodm_db:account_id_key(AID0),
    Tab = table(AID),
    Name = exodm_db:encode_id(Name0),
    Key = exodm_db:join_key(Name, <<"members">>),
    exodm_db:in_transaction(
      fun(_) ->
	      case exists(Tab, Name) of
		  true ->
		      lists:foreach(
			fun(DID0) ->
				DID = exodm_db:encode_id(DID0),
				case exodm_db_device:exist(AID, DID) of
				    true ->
					write(Tab, Key, DID, <<>>),
					exodm_db_device:add_config_data(
					  AID, DID, Name);
				    false ->
					error({unknown_device, [AID,DID]})
				end
			end, DIDs);
		  false ->
		      error({unknown_config_data, Name})
	      end
      end).

list_config_data_members(Name) ->
    AID = exodm_db_session:get_aid(),
    list_config_data_members(AID, Name).

list_config_data_members(AID, Name0) ->
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


exists(Tab, Name) ->
    Key = exodm_db:join_key([exodm_db:encode_id(Name), <<"name">>]),
    case exodm_db:read(Tab, Key) of
	{error, not_found} ->
	    false;
	{ok, _} ->
	    true
    end.

validate_config(_Tab, _AID, _Yang, _Protocol, _Values) ->
    %% We don't yet do any validation. FIXME
    ok.

write(Tab, Name, AKey, Value) ->
    Key = exodm_db:join_key([Name, to_binary(AKey)]),
    write(Tab, Key, Value).

write(Tab, Key, Value) ->
    exodm_db:write(Tab, Key, Value).

%% This doesn't actually produce a tree, but a *flattened* tree consisting
%% only of the objects under <<"values">>.
value_tree(Root, Values) ->
    kvdb_conf:flatten_tree(
      #conf_tree{root = Root,
		 tree = [{<<"values">>, to_tree_(Values)}]}).

update_value_tree(Values, #conf_tree{root = Root, tree = T} = Tree) ->
    {<<"values">>, Vs} = lists:keyfind(<<"values">>, 1, T),
    NewVs = update_tree_(Values, Vs),
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
		  {_, As, _} ->
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
    [{1, update_node(K, V, [])}].

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
 	      {to_binary(K), [], V}
       end, Elems);
to_tree_({array, Elems}) ->
    {SubTree, _} = lists:mapfoldl(
 		     fun({T,_} = X, P) when T==array; T==struct ->
 			     {{P, to_tree_(X)}, P+1};
 			({K,V}, P) ->
 			     {{P, to_tree_({K,V})}, P+1};
			(X, P) ->
			     {{P, [], X}, P+1}
 		     end, 1, Elems),
    SubTree;
to_tree_({K,V}) ->
    [{K, [], V}].



write_values(Tab, Name, Values) ->
    Key = exodm_db:join_key([Name, <<"values">>]),
    write_values_(Tab, Key, Values).

write_values_(Tab, Key, {struct, Elems}) ->
    lists:foreach(
      fun({K,{array,_} = A}) ->
	      Key1 = exodm_db:join_key(Key, to_id(K)),
	      write_values_(Tab, Key1, A);
	 ({K, {struct,_} = S}) ->
	      Key1 = exodm_db:join_key(Key, to_id(K)),
	      write_values_(Tab, Key1, S);
	 ({K,V}) ->
	      Key1 = exodm_db:join_key(Key, to_id(K)),
	      write(Tab, Key1, V)
      end, Elems);
write_values_(Tab, Key, {array, Elems}) ->
    lists:foldl(
      fun({array,__} = A, I) ->
	      Key1 = exodm_db:list_key(Key, I),
	      write_values_(Tab, Key1, A),
	      I+1;
	 ({struct,_} = S, I) ->
	      Key1 = exodm_db:list_key(Key, I),
	      write_values_(Tab, Key1, S),
	      I+1;
	 ({K, V}, I) ->
	      Key1 = exodm_db:join_key(
		       exodm_db:list_key(Key, I), to_id(K)),
	      write(Tab, Key1, V),
	      I+1
      end, 1, Elems).


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

to_id(X) ->
    exodm_db:encode_id(to_binary(X)).

check_access(AID) ->
    AID = exodm_db_session:get_aid().
