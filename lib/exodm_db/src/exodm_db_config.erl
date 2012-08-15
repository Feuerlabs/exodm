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
		      write_values(Tab, NameKey, Values),
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
                              validate_config(Tab, AID, YangSpec, Protocol, Values),
                              write_values(Tab, NameKey, Values),
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
