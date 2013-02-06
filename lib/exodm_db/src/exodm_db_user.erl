%%% @author Tony Rogvall <tony@rogvall.se>
%%% @author Ulf Wiger <ulf@feuerlabs.com>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense user manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_user).

-include_lib("lager/include/log.hrl").

-export([init/0,
	 table/0]).
-export([new/2, 
	 update/2, 
	 lookup/1, 
	 lookup_attr/2,
	 list_users/2,    %% (N, Prev)
	 list_user_keys/0,
	 exist/1,
	 fold_users/2,
	 add_role/3,
	 remove_role/3,
	 list_accounts/1, 
	 list_roles/1, 
	 delete/1]).
-export([subscribe/1,
	 unsubscribe/1]).
-export([add_alias/2,
	 lookup_by_alias/1]).
-export([ix_alias/1]).
-import(exodm_db, [write/2, binary_opt/2, binary_opt/3, to_binary/1]).

-define(TAB, <<"user">>).

%% Role field should be renamed in database FIXME
-define(USER_ROLE, <<"access">>).

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
    ?TAB.

%% Gproc pub/sub ----------------------
subscribe(Event) when Event==add; Event==delete ->
    gproc:reg({p, l, {?MODULE, Event}}).

unsubscribe(Event) when Event==add; Event==delete ->
    catch gproc:unreg({p, l, {?MODULE, Event}}),
    ok.

publish(Event, Data) when Event==add; Event==delete ->
    gproc:send({p, l, {?MODULE, Event}}, {?MODULE, Event, Data}).
%% END Gproc pub/sub ------------------


%%--------------------------------------------------------------------
%% @doc
%% Creates the user in the database.
%%
%% @end
%%--------------------------------------------------------------------
-spec new(UName::binary(), Options::list(tuple())) -> 
		 {ok, Key::term()} |
		 {error, Reason::term()}.

new(UName, Options) ->
    exodm_db:in_transaction(
      fun(_) ->
              case lookup(UName) of
		  [] ->
                      case new_(UName, Options) of
                          {ok, UserName} ->
                              publish(add, UserName),
                              ok;
                          Other ->
                              Other
                      end;
                  User ->
 		      ?debug("found ~p",[User]),
		      error('object-exists')
              end
     end).

new_(UName, Options) ->
    lager:debug("uname ~p, options ~p~n", [UName, Options]),
    [_] = exodm_db:nc_key_split(exodm_db:decode_id(UName)),  %% validation!
    Key = exodm_db:encode_id(UName),
    Name = binary_opt(name, Options, UName),
    case exist_(?TAB, Key) of
	true ->
	    {error, exists};
	false ->
	    insert(Key,name, Name),
	    insert(Key,fullname,      binary_opt(fullname,Options)),
	    insert(Key,phone,         binary_opt(phone,Options)),
	    insert(Key,email,         binary_opt(email,Options)),
	    insert(Key,skype,         binary_opt(skype,Options)),
	    insert_password(Key, password,
			    binary_opt(password, Options)),
	    process_list_options(alias, Key, Options),
	    {ok, exodm_db:decode_id(Key)}
    end.
    

%%--------------------------------------------------------------------
%% @doc
%% Removes the user from the database.
%%
%% @end
%%--------------------------------------------------------------------
-spec delete(UName::binary()) -> ok | {error, not_found}.

delete(UName) ->
    exodm_db:in_transaction(
      fun(_) ->
	      case lookup(UName) of
		  [] ->
		      error('object-not-found');
		  _User ->
                      case delete_(UName) of
                          {ok, UserName} ->
                              publish(delete, UserName),
                              ok;
                          Other ->
                              Other
                      end
	      end
      end).


delete_(UID) ->
    lager:debug("uid ~p~n", [UID]),
    Key = exodm_db:encode_id(UID),
    lager:debug("key ~p~n", [Key]),
    case exist_(?TAB, Key) of
	true ->
	    lager:debug("uid ~p exists ~n", [UID]),
	    kvdb_conf:delete_tree(?TAB, Key),
	    %% Make sure no old session exists if new user
	    %% with same name is created before session timeout.
	    %% exodm_db_session:remove_user_session(UID),
	    {ok, exodm_db:decode_id(Key)};
	false ->
	    {error, not_found}
    end.

process_list_options(O, Key, Options) ->
    Found = exodm_db:list_options(O, Options),
    F = case O of
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

%%--------------------------------------------------------------------
%% @doc
%% Adds a role to the user.
%%
%% Can throw errors: no_such_user, no_such_account
%% @end
%%--------------------------------------------------------------------
-spec add_role(AID::binary(), UName::binary(), RName::binary()) ->
		      ok |
		      {error, Reason::term()}.
add_role(AID, UName, RName) ->
    exodm_db:in_transaction(
      fun(_) ->
	      t_add_role(AID, UName, RName)
      end).

t_add_role(AID0, UName0, RName) ->
    AID = exodm_db:account_id_key(AID0),
    UName = exodm_db:encode_id(UName0),
    case exodm_db_account:exist(AID) of
	true ->
	    case exist(UName) of
		true ->
		    insert_role(UName, AID, RName);
		false ->
		    error({no_such_user, UName})
	    end;
	false ->
	    error({no_such_account, AID})
    end.

insert_role(User, AID, ARole) ->
    ?debug("user ~p, aid ~p, role ~p", [User, AID, ARole]),
    UID = exodm_db:encode_id(User),
    Pos = new_list_pos(Base = exodm_db:join_key(UID, ?USER_ROLE)),
    insert_role_(Base, Pos, AID, ARole).

insert_role_(Base, Pos, AID, ARole) ->
    ?debug("base ~p, pos ~p, aid ~p, role ~p", [Base, Pos, AID, ARole]),
    K = exodm_db:list_key(Base, Pos),
    insert(K, '__aid', AID),
    insert(K, '__rid', exodm_db:encode_id(ARole)).

%%--------------------------------------------------------------------
%% @doc
%% Removes a role from the user.
%%
%% Can throw errors: no_such_user, no_such_account
%% @end
%%--------------------------------------------------------------------
-spec remove_role(AID::binary(), UName::binary(), RName::binary()) ->
		      ok |
		      {error, Reason::term()}.

remove_role(AID, UName, Role) ->
    exodm_db:in_transaction(
      fun(_) ->
	      t_remove_role(AID, UName, Role)
      end).

t_remove_role(AID0, UName0, Role) ->
    AID = exodm_db:account_id_key(AID0),
    UName = exodm_db:encode_id(UName0),
    case exodm_db_account:exist(AID) of
	true ->
	    case exist(UName) of
		true ->
		    delete_role(UName, AID, Role);
		false ->
		    error({no_such_user, UName})
	    end;
	false ->
	    error({no_such_account, AID})
    end.

delete_role(UName, AID, Role) ->
    ?debug("user ~p, aid ~p", [UName, AID]),
    lists:map(fun({I, A, R}) when A == AID, R == Role ->
		      delete_role_(exodm_db:join_key(UName, ?USER_ROLE), I, AID);
		 (_Other) -> ok
	      end, list_roles_(UName)).
    
delete_role_(Base, Pos, AID) ->
    ?debug("base ~p, pos ~p, aid ~p", [Base, Pos, AID]),
    K = exodm_db:list_key(Base, Pos),
    ?debug("k ~p", [K]),
    delete(K, '__aid'),
    delete(K, '__rid').

%%--------------------------------------------------------------------
%% @doc
%% List a users accounts.
%%
%% Can throw errors: no_such_user
%% @end
%%--------------------------------------------------------------------
-spec list_accounts(UName::binary()) ->
                        list(AID::binary()) |
                        {error, Reason::term()}.

list_accounts(UID) when is_binary(UID) ->
    lists:usort([A || {_I, A} <- list_accounts_(UID)]).

list_accounts_(UID)  ->
    exodm_db:fold_list(
      ?TAB,
      fun(I, Key, Acc) ->
    	      case read(?TAB, Key, '__aid') of
    		  [{_, AID}] ->
    		      [{I, exodm_db:decode_id(AID)} | Acc];
    		  _ -> Acc
    	      end
      end, [], exodm_db:join_key(UID, ?USER_ROLE)).

%%--------------------------------------------------------------------
%% @doc
%% List a users roles.
%%
%% Can throw errors: no_such_user, no_such_account
%% @end
%%--------------------------------------------------------------------
-spec list_roles(UName::binary()) ->
		      list({AID::binary(), Role::binary()}) |
		      {error, Reason::term()}.

list_roles(UID0) when is_binary(UID0) ->
    UID = exodm_db:encode_id(UID0),
    [{A, R} || {_I, A, R} <- list_roles_(UID)].

list_roles_(UID)  ->
    exodm_db:fold_list(
      ?TAB,
      fun(I, Key, Acc) ->
    	      case {read(?TAB, Key, '__aid'),
    		    read(?TAB, Key, '__rid')} of
    		  {[{_, AID}], [{_, Role}]} ->
    		      [{I, 
			exodm_db:decode_id(AID), 
			exodm_db:decode_id(Role)} |
		       Acc];
    		  _ -> Acc
    	      end
      end, [], exodm_db:join_key(UID, ?USER_ROLE)).

%%--------------------------------------------------------------------
%%
%% Utils
%%
%%--------------------------------------------------------------------
insert_password(Key, Item, Value) ->
    {ok, Salt} = bcrypt:gen_salt(),
    %% Expensive hash; expensive to create, expensive to check against
    {ok, Hash} = bcrypt:hashpw(Value, Salt),
    insert(Key, Item, to_binary(Hash)).

new_list_pos(Base) ->
    {ok, Last} = kvdb_conf:last_list_pos(?TAB, Base),
    Last+1.

insert(Key, Item, Value) ->
    Key1 = exodm_db:join_key([Key, to_binary(Item)]),
    exodm_db:write(?TAB, Key1, Value).

delete(Key, Item) ->
    Key1 = exodm_db:join_key([Key, to_binary(Item)]),
    kvdb_conf:delete(?TAB, Key1).

read(Tab, Key, Item) ->
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
