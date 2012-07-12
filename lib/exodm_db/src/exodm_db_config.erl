%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     General config model
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_config).

-export([init/1]).
-export([new_config_data/3,
	 new_config_data/4,
	 create_yang_module/3]).
%%
%% /u<UID>/devices/x<DID>/config/<target>/<tree>
%%

init(AID) ->
    exodm_db:in_transaction(
      fun(_) ->
	      kvdb_conf:add_table(table(AID), [])
      end).

table(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    <<AID/binary, "_config">>.

new_config_data(Name, Yang, Values) ->
    AID = exodm_db_session:get_aid(),
    new_config_data(AID, Name, Yang, Values).

new_config_data(AID, Name0, Yang0, Values) ->
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
		      validate_config(Tab, AID, Yang, Values),
		      write(Tab, NameKey, <<"name">>, Name),
		      write(Tab, NameKey, <<"yang">>, Yang),
		      write_values(Tab, NameKey, Values),
		      {ok, Name}
	      end
      end).

create_yang_module(Repository0, File, Yang0) ->
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

exists(Tab, Name) ->
    Key = exodm_db:kvdb_key_join([exodm_db:encode_id(Name), <<"name">>]),
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
    Key = exodm_db:kvdb_key_join([Name, to_binary(AKey)]),
    write(Tab, Key, Value).

write(Tab, Key, Value) ->
    exodm_db:write(Tab, Key, Value).

write_values(Tab, Name, Values) ->
    Key = exodm_db:kvdb_key_join([Name, <<"values">>]),
    write_values_(Tab, Key, Values).

write_values_(Tab, Key, {struct, Elems}) ->
    lists:foreach(
      fun({K,{array,_} = A}) ->
	      Key1 = exodm_db:kvdb_key_join(Key, to_binary(K)),
	      write_values_(Tab, Key1, A);
	 ({K, {struct,_} = S}) ->
	      Key1 = exodm_db:kvdb_key_join(Key, to_binary(K)),
	      write_values_(Tab, Key1, S);
	 ({K,V}) when is_integer(V) ->
	      write(Tab, Key, K, list_to_binary(integer_to_list(V)));
	 ({K,V}) ->
	      write(Tab, Key, K, to_binary(V))
      end, Elems);
write_values_(Tab, Key, {array, Elems}) ->
    lists:foldl(
      fun({array,__} = A, I) ->
	      Key1 = exodm_db:list_key(Key, I),
	      write_values(Tab, Key1, A),
	      I+1;
	 ({struct,_} = S, I) ->
	      Key1 = exodm_db:list_key(Key, I),
	      write_values(Tab, Key1, S),
	      I+1;
	 ({K, V}, I) ->
	      Key1 = exodm_db:kvdb_key_join(exodm_db:list_key(Key, I),
					    to_binary(K)),
	      V1 = if is_integer(V) -> list_to_binary(integer_to_list(V));
		      true -> to_binary(V)
		   end,
	      write(Tab, Key1, V1),
	      I+1
      end, 1, Elems).

to_binary(X) when is_atom(X) -> atom_to_binary(X, latin1);
to_binary(X) when is_binary(X) -> X;
to_binary(X) when is_list(X) -> list_to_binary(X).

