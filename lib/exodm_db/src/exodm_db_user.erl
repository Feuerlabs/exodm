%%% @author Tony Rogvall <tony@rogvall.se>
%%% @author Ulf Wiger <ulf@feuerlabs.com>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense user manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_user).

-export([init/0]).
-export([new/4, update/2, lookup/1, lookup_attr/2,
	 list_user_keys/0, list_user_keys/1,
	 fold_users/2, fold_users/3,
	 list_access/1, exist/1]).
-export([add_alias/2,
	 lookup_by_alias/1]).
-export([ix_alias/1]).
-import(exodm_db, [write/2, binary_opt/2, binary_opt/3, to_binary/1]).

-define(TAB, <<"user">>).
%%
%% /account/<AID>/user/<UID>
%% /account/<AID>/user/<UID>/'__uid'    = User ID
%% /account/<AID>/user/<UID>/'__aid'    = Account ID
%% /account/<AID>/user/<UID>/name       = ShortName
%% /account/<AID>/user/<UID>/fullname   = Name|Company
%% /account/<AID>/user/<UID>/phone      = Phone number to contact
%% /account/<AID>/user/<UID>/email      = Email to contact
%% /account/<AID>/user/<UID>/skype      = Skype ID of contact
%% /account/<AID>/user/<UID>/__password = password - stored encrypted!
%% /account/<AID>/user/<UID>/access[<u>]/__aid  = Device owner ID
%% /account/<AID>/user/<UID>/access[<u>]/__gid  = Device group ID
%% /account/<AID>/user/<UID>/access[<u>]/__perm = Device group access
%%
init() ->
    exodm_db:add_table(<<"user">>, [alias]).

new(AID0, UName, Role, Options) ->
    [_] = exodm_db:nc_key_split(exodm_db:decode_id(UName)),  %% validation!
    Key = exodm_db:encode_id(UName),
    AID = exodm_db:account_id_key(AID0),
    Name = binary_opt(name, Options, UName),
    case exist_(?TAB, Key) of
	true ->
	    {error, exists};
	false ->
	    insert(?TAB, Key,name, Name),
	    insert(?TAB, Key,'__aid', aid_value(AID)),
	    insert(?TAB, Key,fullname,      binary_opt(fullname,Options)),
	    insert(?TAB, Key,phone,         binary_opt(phone,Options)),
	    insert(?TAB, Key,email,         binary_opt(email,Options)),
	    insert(?TAB, Key,skype,         binary_opt(skype,Options)),
	    insert_password(?TAB, Key, '__password',
			    binary_opt('__password', Options)),
	    lists:foldl(
	      fun({AAID,ARole}, I) ->
		      insert_access(?TAB, Key, I, AAID, ARole)
	      end, 1, [{AID, Role}|proplists:get_all_values(access, Options)]),
	    lists:foldl(
	      fun(Al, I) when is_binary(Al) ->
		      K = exodm_db:kvdb_key_join(
			    Key, exodm_db:list_key(alias, I)),
		      insert(?TAB, K, '__alias', to_binary(Al))
	      end, 1, proplists:get_all_values(alias, Options)),
	    {ok, UName}
    end.

add_alias(UID0, Alias) ->
    UID = exodm_db:encode_id(UID0),
    case exist(UID) of
	true ->
	    case alias_exists(Alias) of
		false ->
		    exodm_db:append_to_list(
		      <<"user">>, exodm_db:kvdb_key_join(UID, <<"alias">>),
		      '__alias', to_binary(Alias));
		true ->
		    error(alias_exists, [UID0, Alias])
	    end;
	false ->
	    error(unknown_user, [UID0, Alias])
    end.

alias_exists(Alias) ->
    case exist(exodm_db:encode_id(Alias)) of
	true ->
	    true;
	false ->
	    case kvdb:index_get(kvdb_conf, ?TAB, alias, Alias) of
		[]  -> false;
		[_] -> true
	    end
    end.

lookup_by_alias(Alias) ->
    case kvdb:index_get(kvdb_conf, ?TAB, alias, Alias) of
	[] ->
	    [];
	[{K, _, _}] ->
	    [UID|_] = exodm_db:kvdb_key_split(K),
	    lookup(UID)
    end.

ix_alias({K, _, V}) ->
    case lists:reverse(exodm_db:kvdb_key_split(K)) of
	[<<"__alias">>,<<"alias[",_/binary>>|_] ->
	    io:fwrite("alias (~p): ~p~n", [K, V]),
	    [V];
	_ ->
	    []
    end.

%% new_uid() ->
%%     exodm_db_system:new_uid().

update(UID, Options) ->
    Tab = <<"user">>,
    Key = key(UID),
    Ops =
	lists:map(
	  fun
	      ({fullname,Value}) ->
		  fun() -> insert(Tab,Key,fullname, to_binary(Value)) end;
	      ({phone,Value}) ->
		  fun() -> insert(Tab,Key,phone,    to_binary(Value)) end;
	      ({email,Value}) ->
		  fun() -> insert(Tab,Key,email,    to_binary(Value)) end;
	      ({skype,Value}) ->
		  fun() -> insert(Tab,Key,skype,    to_binary(Value)) end;
	      %% ({account,Acct}) ->
	      %% 	  Account = to_binary(Acct),
	      %% 	  case exodm_db_system:get_aid_user(Account) of
	      %% 	      {error, not_found} ->
	      %% 		  erlang:error({no_such_aid, Account});
	      %% 	      _ ->
	      %% 		  fun() -> insert(Key, account, Account) end
	      %% 	  end;
	      ({'__password',Value}) ->
		  fun() ->
			  insert_password(Tab,Key, '__password',
					  to_binary(Value))
		  end;
	      ({access, {I,AUID,ARole}}) ->
		  fun() -> insert_access(Tab, Key, I, AUID, ARole) end;
	      ({alias, {I,Al}}) ->
		  fun() -> K = exodm_db:kvdb_key_join(
				 Key, exodm_db:list_key(alias, I)),
			   insert(Tab, K, '__alias', to_binary(Al))
		  end
	  end, Options),
    lists:foreach(fun(Op) -> Op() end, Ops).

lookup(UID) ->
    lookup_(<<"user">>, key(UID)).

%% lookup(AID, UID) ->
%%     case exist(AID, UID) of
%% 	true ->
%% 	    lookup(UID);
%% 	false ->
%% 	    []
%%     end.

lookup_(Tab, Key) ->
    read(Tab, Key,name) ++
	read_uint32(Tab, Key,'__aid') ++
	read(Tab, Key,fullname) ++
	read(Tab, Key,phone) ++
	read(Tab, Key,email) ++
	read(Tab, Key,skype).

lookup_attr(UName, Attr) ->
    Tab = <<"user">>,
    Key = exodm_db:encode_id(UName),
    read(Tab, Key, Attr).

exist(UID) ->
    exist_(?TAB, exodm_db:encode_id(UID)).

exist_(Tab, Key) ->
    case read(Tab, Key,name) of
	[] -> false;
	[_] -> true
    end.


list_user_keys() ->
    list_user_keys(30).

list_user_keys(Limit) ->
    fold_users(fun(UID, Acc) ->
			  [UID|Acc]
		  end, [], Limit).

fold_users(F, Acc) ->
    fold_users(F, Acc, 30).

fold_users(F, Acc, Limit) when
      Limit==infinity; is_integer(Limit), Limit > 0 ->
    exodm_db:fold_keys(
      <<"user">>,
      <<>>,
      fun([UID|_], Acc1) ->
	      {next, UID, F(UID,Acc1)}
      end, Acc, Limit).


key(UID) ->
    exodm_db:encode_id(UID).

aid_value(AID) ->
    <<(exodm_db:account_id_num(AID)):32>>.

insert(Tab, Key, Item, Value) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    exodm_db:write(Tab, Key1, Value).

insert_access(Tab, K0, I, AID0, ARole) when is_integer(I), I>=0 ->
    AID = exodm_db:account_id_key(AID0),
    %% ARole = exodm_db:encode_id(ARole0),
    K = exodm_db:kvdb_key_join(K0, exodm_db:list_key(access, I)),
    insert(Tab, K, '__aid', aid_value(AID)),
    insert(Tab, K, '__role', ARole).

list_access(UID0) when is_binary(UID0) ->
    UID = exodm_db:encode_id(UID0),
    exodm_db:fold_list(
      ?TAB,
      fun(I, Key, Acc) ->
	      case {read_uint32(?TAB, Key, '__aid'),
		    read(?TAB, Key, '__role')} of
		  {[{_, AID}], [{_, Role}]} ->
		      [{I, {AID, Role}}|Acc];
		  _ -> Acc
	      end
      end, [], exodm_db:kvdb_key_join(UID, <<"access">>)).

insert_password(Tab, Key, Item, Value) ->
    {ok, Salt} = bcrypt:gen_salt(),
    %% Expensive hash; expensive to create, expensive to check against
    {ok, Hash} = bcrypt:hashpw(Value, Salt),
    insert(Tab, Key, Item, to_binary(Hash)).

read(Tab, Key,Item) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    case exodm_db:read(Tab, Key1) of
	{ok,{_,_,Value}} ->
	    [{Item,Value}];
	{error,not_found} -> []
    end.

read_uint32(Tab, Key,Item) ->
    case read(Tab, Key,Item) of
	[{Item,<<Value:32>>}] -> [{Item,Value}];
	[] -> []
    end.
