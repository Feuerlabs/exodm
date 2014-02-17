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
-export([new/2, 
	 update/2, 
	 lookup/1, 
	 lookup_attr/2,
	 list_users/2,  
	 list_users/3,  
	 list_user_keys/0,
	 exist/1,
	 fold_users/2,
	 add_role/3,
	 remove_role/3,
	 list_accounts/1, 
	 list_account_roles/2, 
	 list_roles/1, 
	 is_init_admin/1,
	 delete/1, delete/2]).
-export([subscribe/1,
	 unsubscribe/1]).
-export([add_alias/2,
	 lookup_by_alias/1]).
-export([ix_alias/1]).
-import(exodm_db, [write/2, to_binary/1]).

-define(TAB, <<"user">>).

-include_lib("lager/include/log.hrl").
-include("exodm.hrl").
-include("exodm_db.hrl").


init() ->
    exodm_db:in_transaction(
      fun(_) ->
	      exodm_db:add_table(table(), [alias, aid])
      end).

table() ->
    ?TAB.

%% Gproc pub/sub ----------------------
subscribe(Event) when Event==add; Event==delete ->
    ?debug("~p subscribes",[self()]),
    gproc:reg({p, l, {?MODULE, Event}}).

unsubscribe(Event) when Event==add; Event==delete ->
    ?debug("~p unsubscribes",[self()]),
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
    case valid_name(UName) of
	true ->
	    exodm_db:in_transaction(
	      fun(_) ->
		      case exist(UName) of  
			  true ->
			      ?debug("found ~p",[UName]),
			      error(?OBJECT_EXISTS);
			  false ->
			      case new_(UName, Options) of
				  {ok, UserName} ->
				      lager:debug("user ~p created", [UserName]),
				      publish(add, UserName),
				      ok;
				  Other ->
				      lager:debug(
					"new result ~p returned", [Other]),
				      Other
			      end
		      end
	      end);
	false ->
	    error(?ILLEGAL_NAME)
    end.

valid_name(<<C,_/binary>>) ->
    not lists:member(C, exodm_db:delimiters());
valid_name(_) ->
    false.


new_(UName, Options) ->
    lager:debug("uname ~p, options ~p~n", [UName, Options]),
    %% validation!
    case exodm_db:nc_key_split(exodm_db:decode_id(UName)) of
	[_] -> 
	    %% Normal case
	    ok; 
	[Account, _Name] ->
	    %% User name of type Account/Name, verify that
	    %% Account exists
	    case exodm_db_account:lookup_by_name(Account) of
		AID when is_binary(AID) -> ok;
		false -> error(?ILLEGAL_NAME)
	    end;
	_ -> 
	    error(?ILLEGAL_NAME)
    end,
    Key = exodm_db:encode_id(UName),
    case proplists:get_value(?USER_OPT_PASSWORD, Options) of
        undefined -> error(?MISSING_OPTION);
        Pwd ->insert_password(Key, ?USER_DB_PASSWORD, Pwd)
    end,
    db_insert(Key,?USER_DB_NAME, UName),
    db_insert(Key,?USER_DB_FULLNAME, 
           proplists:get_value(?USER_OPT_FNAME,Options, <<>>)),
    db_insert(Key,?USER_DB_PHONE,    
           proplists:get_value(?USER_OPT_PHONE,Options, <<>>)),
    db_insert(Key,?USER_DB_EMAIL,    
           proplists:get_value(?USER_OPT_EMAIL,Options, <<>>)),
    db_insert(Key,?USER_DB_SKYPE,    
           proplists:get_value(?USER_OPT_SKYPE,Options, <<>>)),
    process_list_options(?USER_OPT_ALIAS, Key, Options),
    {ok, exodm_db:decode_id(Key)}.
    

%%--------------------------------------------------------------------
%% @doc
%% Removes the user from the database.
%%
%% @end
%%--------------------------------------------------------------------
-spec delete(UName::binary()) -> ok | {error, not_found}.

delete(UName) ->
    delete(UName, false).

-spec delete(UName::binary(), Forced::boolean()) -> 
		    ok | {error, not_found}.

delete(UName, Forced) ->
    exodm_db:in_transaction(
      fun(_) ->
	      case exist(UName) of 
		  false ->
		      error(?OBJECT_NOT_FOUND);
		  true ->
		      case {is_init_admin(UName), Forced} of
			  {true, false} -> 
			      error(?PERMISSION_DENIED);
			  _OkToDelete ->
			      {ok, UserName} = delete_(UName),
			      lager:debug("user ~p deleted", [UserName]),
			      publish(delete, UserName),
			      ok
		      end
	      end
      end).

delete_(UName) ->
    lager:debug("uid ~p~n", [UName]),
    Key = exodm_db:encode_id(UName),
    lager:debug("key ~p~n", [Key]),
    remove_account_access(UName),
    kvdb_conf:delete_tree(?TAB, Key),
    %% Make sure no old session exists if new user
    %% with same name is created before session timeout.
    %% exodm_db_session:remove_user_session(UName),
    {ok, exodm_db:decode_id(Key)}.

is_init_admin(UName) ->
    lists:any(fun({_A, Role}) ->
		      case Role of
			  ?INIT_ADMIN -> true;
			  _ -> false
		      end
	      end, list_roles(UName)).
				       
remove_account_access(UName) ->
    remove_account_access(UName, list_roles(UName)).

remove_account_access(_UName, []) ->
    ok;
remove_account_access(UName, [{AID, Role} | Rest]) ->
    %% Hmm, must we have ISroot as true to enforce deletion ???
    exodm_db_account:remove_users(AID, Role, [UName], true),
    remove_account_access(UName, Rest).

process_list_options(O, Key, Options) ->
    Found = exodm_db:list_options(O, Options),
    F = case O of
	    ?USER_OPT_ALIAS ->
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
		      ?TAB, exodm_db:join_key(UID, <<"alias">>),
		      fun(LKey) ->
			      db_insert(LKey, ?USER_DB_ALIAS, to_binary(Alias))
		      end);
		true ->
		    ?ei("alias_exists ~p ~p", [UID0, Alias]),
		    error(?OBJECT_EXISTS)
	    end;
	false ->
	    error(?OBJECT_NOT_FOUND, [UID0, Alias])
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


%%--------------------------------------------------------------------
update(UName, Options) when is_binary(UName)->
    exodm_db:in_transaction(
      fun(_) ->
	      case exist(UName) of 
		  false ->
		      error(?OBJECT_NOT_FOUND);
		  true ->
		      update_(UName, Options)
	      end
      end).

update_(UName, Options) ->
    ?debug("user ~p, options ~p", [UName, Options]),
    Key = exodm_db:encode_id(UName),
    F=fun
	  ({?USER_OPT_FNAME,Value}) ->
	      db_insert(Key,?USER_DB_FULLNAME, Value);
	  ({?USER_OPT_PHONE,Value}) ->
	      db_insert(Key,?USER_DB_PHONE,    Value);
	  ({?USER_OPT_EMAIL,Value}) ->
	      db_insert(Key,?USER_DB_EMAIL,    Value);
	  ({?USER_OPT_SKYPE,Value}) ->
	      db_insert(Key,?USER_DB_SKYPE,    Value);
	  ({?USER_OPT_PASSWORD,Value}) ->
	      insert_password(Key, ?USER_DB_PASSWORD, Value);
	  (_) ->
	      ignore
      end,
    lists:foreach(F, Options),
    process_list_options(?USER_OPT_ALIAS, Key, Options).

%% read_uint32(Tab, Key,'__aid') ++ 
lookup(UName) ->
    Key = exodm_db:encode_id(UName),
    lookup_attr_(?TAB,Key,name) ++
    lookup_attr_(?TAB,Key,fullname) ++
    lookup_attr_(?TAB,Key,phone) ++
    lookup_attr_(?TAB,Key,email) ++
    lookup_attr_(?TAB,Key,skype).

lookup_attr(UID, Attr) when is_atom(Attr) ->
    lookup_attr_(?TAB, exodm_db:encode_id(UID), Attr).

lookup_attr_(Tab, UID, Attr) when is_atom(Attr) ->
    case read_value(Tab, UID, atom_to_binary(Attr,latin1)) of
	false -> [];
	Value -> [{Attr,Value}]
    end.
    

exist(UID) ->
    exist_(?TAB, exodm_db:encode_id(UID)).

exist_(Tab, UID) ->
    case read_value(Tab,UID,?USER_DB_NAME) of
	false -> false;
	_ -> true
    end.

list_users(N, Prev, ?ASC) ->
    list_next_users(N, Prev);
list_users(N, Next, ?DESC) ->
    list_prev_users(N, Next).
    
list_users(N, Prev) ->
    list_next_users(N, Prev).
    
list_next_users(N, Prev) ->
    exodm_db:in_transaction(
      fun(_) ->
	      exodm_db:list_next(table(),
				 N, exodm_db:to_binary(Prev),
				 fun(Key) ->
					 lookup(Key)
				 end)
      end).

list_prev_users(N, Next) ->
    exodm_db:in_transaction(
      fun(_) ->
	      exodm_db:list_prev(table(),
				 N, exodm_db:to_binary(Next),
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
      ?TAB, F, Acc, <<>>).


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
		    error(?OBJECT_NOT_FOUND)
	    end;
	false ->
	    error(?OBJECT_NOT_FOUND)
    end.

insert_role(User, AID, ARole) ->
    ?debug("user ~p, aid ~p, role ~p", [User, AID, ARole]),
    UID = exodm_db:encode_id(User),
    Pos = new_list_pos(Base = exodm_db:join_key(UID, ?USER_ROLE)),
    insert_role_(Base, Pos, AID, ARole).

insert_role_(Base, Pos, AID, ARole) ->
    ?debug("base ~p, pos ~p, aid ~p, role ~p", [Base, Pos, AID, ARole]),
    K = exodm_db:list_key(Base, Pos),
    db_insert(K, ?USER_DB_ROLE_AID, AID),
    db_insert(K, ?USER_DB_ROLE_RID, exodm_db:encode_id(ARole)).

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
		    error(?OBJECT_NOT_FOUND)
	    end;
	false ->
	    error(?OBJECT_NOT_FOUND)
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
    db_delete(K, ?USER_DB_ROLE_AID),
    db_delete(K, ?USER_DB_ROLE_RID).

%%--------------------------------------------------------------------
%% @doc
%% List a users accounts.
%%
%% Can throw errors: no_such_user
%% FIXME: documenation ! is this still true?
%% @end
%%--------------------------------------------------------------------
-spec list_accounts(UName::binary()) ->
                        list(AID::binary()) |
                        {error, Reason::term()}.

list_accounts(UID) when is_binary(UID) ->
    lists:usort([A || {_I, A} <- list_accounts_(UID)]);
list_accounts(UID) when is_list(UID) ->
    list_accounts(to_binary(UID)).

list_accounts_(UID)  ->
    exodm_db:fold_list(
      ?TAB,
      fun(I, Key, Acc) ->
    	      case read_value(?TAB, Key, ?USER_DB_ROLE_AID) of
		  false -> Acc;
		  AID -> [{I, exodm_db:decode_id(AID)} | Acc]
    	      end
      end, [], exodm_db:join_key(exodm_db:encode_id(UID), ?USER_ROLE)).

%%--------------------------------------------------------------------
%% @doc
%% List a users roles.
%%
%% Can throw errors: no_such_user, no_such_account
%% FIXME: documentation ! is this still true?
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
    	      case {read_value(?TAB, Key, ?USER_DB_ROLE_AID),
    		    read_value(?TAB, Key, ?USER_DB_ROLE_RID)} of
		  {false,_} -> Acc;
		  {_,false} -> Acc;
		  {AID,Role} ->
    		      [{I, 
			exodm_db:decode_id(AID), 
			exodm_db:decode_id(Role)} |
		       Acc]
    	      end
      end, [], exodm_db:join_key(UID, ?USER_ROLE)).

%%--------------------------------------------------------------------
%% @doc
%% List a users roles for a certain account.
%%
%% Can throw errors: no_such_user, no_such_account
%% FIXME: documenation ! is this still true?
%% @end
%%--------------------------------------------------------------------
-spec list_account_roles(AID::binary(), UName::binary()) ->
                                list(Role::binary()) |
		      {error, Reason::term()}.

list_account_roles(AID, UName) when is_binary(AID), is_binary(UName) ->
    [R || {A, R} <- list_roles(UName), A == AID].


%%--------------------------------------------------------------------
%%
%% Utils
%%
%%--------------------------------------------------------------------
insert_password(Key, Item, Value) ->
    ?debug("key ~p, item ~p, value ******** ", [Key, Item]),
    {ok, Salt} = bcrypt:gen_salt(),
    %% Expensive hash; expensive to create, expensive to check against
    %% So make sure we can not use this fact for a DOS attack!
    {ok, Hash} = bcrypt:hashpw(Value, Salt),
    db_insert(Key, Item, to_binary(Hash)).

new_list_pos(Base) ->
    {ok, Last} = kvdb_conf:last_list_pos(?TAB, Base),
    Last+1.

db_insert(Key, Item, Value) when is_binary(Item) ->
    ?debug("db_insert: key ~p, item ~p, value ~p ", [Key, Item, Value]),
    Key1 = exodm_db:join_key([Key, Item]),
    exodm_db:write(?TAB, Key1, Value).

db_delete(Key, Item) when is_binary(Item) ->
    Key1 = exodm_db:join_key([Key, Item]),
    kvdb_conf:delete(?TAB, Key1).


read_value(Tab, Key, Item) when is_binary(Item) ->
    Key1 = exodm_db:join_key([Key, Item]),
    case exodm_db:read(Tab, Key1) of
	{ok,{_,_,Value}} -> Value;
	{error,not_found} -> false
    end.

%%read_uint32(Tab, Key,Item) ->
%%    case read(Tab, Key,Item) of
%%	[{Item,<<Value:32>>}] -> [{Item,Value}];
%%	[] -> []
%%    end.
