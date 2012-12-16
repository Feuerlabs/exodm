%%% @author Tony Rogvall <tony@rogvall.se>
%%% @author Ulf Wiger <ulf@feuerlabs.com>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense user manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_user).

-export([init/0,
	 table/0]).
-export([new/3, update/2, lookup/1, lookup_attr/2,
	 list_users/2,    %% (N, Prev)
	 list_user_keys/0, fold_users/2,
	 add_access/3,
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
%% /account/<AID>/user/<UID>/password   = password - stored encrypted!
%% /account/<AID>/user/<UID>/access[<u>]/__aid  = Device owner ID
%% /account/<AID>/user/<UID>/access[<u>]/__gid  = Device group ID
%% /account/<AID>/user/<UID>/access[<u>]/__perm = Device group access
%%
init() ->
    exodm_db:in_transaction(
      fun(_) ->
	      exodm_db:add_table(table(), [alias, aid])
      end).

table() ->
    <<"user">>.

new(AID, UName, Options) ->
    exodm_db:in_transaction(
      fun(_) ->
	      new_(AID, UName, Options)
      end).

new_(AID0, UName, Options) ->
    [_] = exodm_db:nc_key_split(exodm_db:decode_id(UName)),  %% validation!
    Key = exodm_db:encode_id(UName),
    AID = exodm_db:account_id_key(AID0),
    Name = binary_opt(name, Options, UName),
    case exist_(?TAB, Key) of
	true ->
	    {error, exists};
	false ->
	    insert(Key,name, Name),
	    insert(Key,'__aid',   exodm_db:account_id_value(AID)),
	    exodm_db_account:add_user(AID, Key),
	    insert(Key,fullname,      binary_opt(fullname,Options)),
	    insert(Key,phone,         binary_opt(phone,Options)),
	    insert(Key,email,         binary_opt(email,Options)),
	    insert(Key,skype,         binary_opt(skype,Options)),
	    insert_password(Key, password,
			    binary_opt(password, Options)),
	    process_list_options(access, Key, Options),
	    process_list_options(alias, Key, Options),
	    %% lists:foldl(
	    %%   fun(Al, I) when is_binary(Al) ->
	    %% 	      K = exodm_db:join_key(
	    %% 		    Key, exodm_db:list_key(alias, I)),
	    %% 	      insert(?TAB, K, '__alias', to_binary(Al))
	    %%   end, 1, proplists:get_all_values(alias, Options)),
	    {ok, UName}
    end.

process_list_options(O, Key, Options) ->
    Found = exodm_db:list_options(O, Options),
    F = case O of
	    access ->
		fun({I, {AAID,ARole}}) ->
			insert_access(Key, I, AAID, ARole)
		end;
	    alias ->
		fun({I, Alias}) ->
			exodm_db:insert_alias(?TAB, Key, I, Alias)
		end
	end,
    lists:foreach(F, Found).


add_alias(UID, Alias) ->
    exodm_db:in_transaction(
      fun(_) ->
	      add_alias_(UID, Alias)
      end).

add_alias_(UID0, Alias) ->
    UID = exodm_db:encode_id(UID0),
    case exist(UID) of
	true ->
	    case alias_exists(Alias) of
		false ->
		    exodm_db:append_to_list(
		      <<"user">>, exodm_db:join_key(UID, <<"alias">>),
		      fun(LKey) ->
			      insert(LKey, '__alias', to_binary(Alias))
		      end);
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
	    [UID|_] = exodm_db:split_key(K),
	    lookup(UID)
    end.

ix_alias({K, _, V}) ->
    case lists:reverse(exodm_db:split_key(K)) of
	[<<"__alias">>,<<"alias[",_/binary>>|_] ->
	    io:fwrite("alias (~p): ~p~n", [K, V]),
	    [V];
	_ ->
	    []
    end.

%% new_uid() ->
%%     exodm_db_system:new_uid().

update(UID, Options) ->
    exodm_db:in_transaction(
      fun(_) ->
	      update_(UID, Options)
      end).

update_(UID, Options) ->
    Key = key(UID),
    F=fun
	  ({fullname,Value}) ->
	      insert(Key,fullname, to_binary(Value));
	  ({phone,Value}) ->
	      insert(Key,phone,    to_binary(Value));
	  ({email,Value}) ->
	      insert(Key,email,    to_binary(Value));
	  ({skype,Value}) ->
	      insert(Key,skype,    to_binary(Value));
	  %% ({account,Acct}) ->
	  %% 	  Account = to_binary(Acct),
	  %% 	  case exodm_db_system:get_aid_user(Account) of
	  %% 	      {error, not_found} ->
	  %% 		  erlang:error({no_such_aid, Account});
	  %% 	      _ ->
	  %% 		  insert(Key, account, Account)
	  %% 	  end;
	  ({password,Value}) ->
	      insert_password(Key, password,
			      to_binary(Value));
	  (_) ->
	      ignore
      end,
    lists:foreach(F, Options),
    process_list_options(access, Key, Options),
    process_list_options(alias, Key, Options).

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

list_users(N, Prev) ->
    exodm_db:in_transaction(
      fun(_) ->
	      exodm_db:list_next(table(),
				 N, exodm_db:to_binary(Prev),
				 fun(Key) ->
					 lookup(Key)
				 end)
      end).


list_user_keys() ->
    lists:reverse(fold_users(fun(UID, Acc) ->
				     [UID|Acc]
			     end, [])).

fold_users(F, Acc) ->
    kvdb_conf:fold_children(
      <<"user">>, F, Acc, <<>>).


key(UID) ->
    exodm_db:encode_id(UID).

insert(Key, Item, Value) ->
    Key1 = exodm_db:join_key([Key, to_binary(Item)]),
    exodm_db:write(?TAB, Key1, Value).

add_access(AID, UName, RID) ->
    exodm_db:in_transaction(
      fun(_) ->
	      t_add_access(AID, UName, RID)
      end).

t_add_access(AID0, UName0, RID) ->
    AID = exodm_db:account_id_key(AID0),
    UName = exodm_db:encode_id(UName0),
    case exodm_db_account:exist(AID) of
	true ->
	    case exist(UName) of
		true ->
		    %% insert(?TAB, exodm_db:join_key(
		    %% 		   [UName, <<"access">>, AID]), '__rid',
		    %% 	   exodm_db:role_id_value(RID));
		    insert_access(UName, AID, RID);
		false ->
		    error({no_such_user, UName})
	    end;
	false ->
	    error({no_such_account, AID})
    end.

%% add_access_(AID, UName, RID) ->
%%     Pos = exodm_db:append_to_list(
%% 	    ?TAB, exodm_db:join_key(UName, <<"access">>),
%% 	    '__aid', exodm_db:account_id_value(AID)),
%%     insert(?TAB, exodm_db:join_key(
%% 		   UName, exodm_db:list_key(access, Pos)),
%% 	   '__rid', exodm_db:role_id_value(RID)).

insert_access(User, AID, ARole) ->
    UID = exodm_db:encode_id(User),
    Pos = new_list_pos(Base = exodm_db:join_key(UID, <<"access">>)),
    insert_access_(Base, Pos, AID, ARole).

new_list_pos(Base) ->
    {ok, Last} = kvdb_conf:last_list_pos(?TAB, Base),
    Last+1.

insert_access(User, I, AID, ARole) when is_integer(I), I>=0 ->
    UID = exodm_db:encode_id(User),
    insert_access_(exodm_db:join_key(UID, <<"access">>), I, AID, ARole).

insert_access_(Base, Pos, AID, ARole) ->
    K = exodm_db:list_key(Base, Pos),
    insert(K, '__aid', exodm_db:account_id_value(AID)),
    insert(K, '__rid', exodm_db:role_id_value(ARole)).

list_access(UID0) when is_binary(UID0) ->
    UID = exodm_db:encode_id(UID0),
    %% {Found, _} = kvdb_conf:prefix_match(
    %% 		   ?TAB, exodm_db:join_key(UID, <<"access">>), infinity),
    %% lists:flatmap(
    %%   fun({K,_,V}) ->
    %% 	      case kvdb_conf:raw_split_key(K) of
    %% 		  [_,_,AID,<<"__rid">>] ->
    %% 		      [{AID, exodm_db:role_id_key(V)}];
    %% 		  _ ->
    %% 		      []
    %% 	      end
    %%   end, Found).
    exodm_db:fold_list(
      ?TAB,
      fun(I, Key, Acc) ->
    	      case {read_uint32(?TAB, Key, '__aid'),
    		    read(?TAB, Key, '__rid')} of
    		  {[{_, AID}], [{_, Role}]} ->
    		      [{I, {AID, Role}}|Acc];
    		  _ -> Acc
    	      end
      end, [], exodm_db:join_key(UID, <<"access">>)).

insert_password(Key, Item, Value) ->
    {ok, Salt} = bcrypt:gen_salt(),
    %% Expensive hash; expensive to create, expensive to check against
    {ok, Hash} = bcrypt:hashpw(Value, Salt),
    insert(Key, Item, to_binary(Hash)).

read(Tab, Key,Item) ->
    Key1 = exodm_db:join_key([Key, to_binary(Item)]),
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
