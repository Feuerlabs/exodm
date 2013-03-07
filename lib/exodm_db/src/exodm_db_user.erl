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
	 list_users/2,    %% (N, Prev)
	 list_user_keys/0,
	 exist/1,
	 fold_users/2,
	 add_role/3,
	 remove_role/3,
	 list_accounts/1, 
	 list_account_roles/2, 
	 list_roles/1, 
	 delete/1]).
-export([subscribe/1,
	 unsubscribe/1]).
-export([add_alias/2,
	 lookup_by_alias/1]).
-export([ix_alias/1]).
-import(exodm_db, [write/2, to_binary/1]).

-define(TAB, <<"user">>).

-include_lib("lager/include/log.hrl").
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
		      case exist(UName) of  %% FIXME! checked twice, also in new_
			  true ->
			      ?debug("found ~p",[UName]),
			      error('object-exists');
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
	    error(invalid_name)
    end.

valid_name(<<C,_/binary>>) ->
    not lists:member(C, exodm_db:delimiters());
valid_name(_) ->
    false.


new_(UName, Options) ->
    lager:debug("uname ~p, options ~p~n", [UName, Options]),
    [_] = exodm_db:nc_key_split(exodm_db:decode_id(UName)),  %% validation!
    Key = exodm_db:encode_id(UName),
    case proplists:get_value(?USER_OPT_PASSWORD, Options) of
        undefined -> error(no_password);
        Pwd ->insert_password(Key, ?USER_DB_PASSWORD, Pwd)
    end,
    insert(Key,?USER_DB_NAME, UName),
    insert(Key,?USER_DB_FULLNAME, 
           proplists:get_value(?USER_OPT_FNAME,Options, <<>>)),
    insert(Key,?USER_DB_PHONE,    
           proplists:get_value(?USER_OPT_PHONE,Options, <<>>)),
    insert(Key,?USER_DB_EMAIL,    
           proplists:get_value(?USER_OPT_EMAIL,Options, <<>>)),
    insert(Key,?USER_DB_SKYPE,    
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
    exodm_db:in_transaction(
      fun(_) ->
	      case exist(UName) of %% FIXME! checked twice, also in delete_
		  false ->
		      error('object-not-found');
		  true ->
                      case delete_(UName) of
                          {ok, UserName} ->
                              lager:debug("user ~p deleted", [UserName]),
                              publish(delete, UserName),
                              ok;
                          Other ->  %% can really not found be returned ?
                              lager:debug("delete result ~p returned", [Other]),
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
			      insert(LKey, ?USER_DB_ALIAS, to_binary(Alias))
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

update(UID, Options) when is_binary(UID)->
    exodm_db:in_transaction(
      fun(_) ->
	      update_(UID, Options)
      end).

update_(UID, Options) ->
    Key = exodm_db:encode_id(UID),
    F=fun
	  ({?USER_OPT_FNAME,Value}) ->
	      insert(Key,?USER_DB_FULLNAME, Value);
	  ({?USER_OPT_PHONE,Value}) ->
	      insert(Key,?USER_DB_PHONE,    Value);
	  ({?USER_OPT_EMAIL,Value}) ->
	      insert(Key,?USER_DB_EMAIL,    Value);
	  ({?USER_OPT_SKYPE,Value}) ->
	      insert(Key,?USER_DB_SKYPE,    Value);
	  ({?USER_OPT_PASSWORD,Value}) ->
	      insert_password(Key, ?USER_DB_PASSWORD, Value);
	  (_) ->
	      ignore
      end,
    lists:foreach(F, Options),
    process_list_options(?USER_OPT_ALIAS, Key, Options).

%% read_uint32(Tab, Key,'__aid') ++ 
lookup(UID0) ->
    UID = exodm_db:encode_id(UID0),
    lookup_attr_(?TAB,UID,name) ++
    lookup_attr_(?TAB,UID,fullname) ++
    lookup_attr_(?TAB,UID,phone) ++
    lookup_attr_(?TAB,UID,email) ++
    lookup_attr_(?TAB,UID,skype).

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
    insert(K, ?USER_DB_ROLE_AID, AID),
    insert(K, ?USER_DB_ROLE_RID, exodm_db:encode_id(ARole)).

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
    delete(K, ?USER_DB_ROLE_AID),
    delete(K, ?USER_DB_ROLE_RID).

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
%% FIXME: documenation ! is this still true?
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
-spec list_account_roles(UName::binary(), AID::binary()) ->
                                list(Role::binary()) |
		      {error, Reason::term()}.

list_account_roles(AID, UID) when is_binary(AID), is_binary(UID) ->
    [R || {_I, R} <- list_account_roles_(AID, UID)].

list_account_roles_(AID, UID)  ->
    exodm_db:fold_list(
      ?TAB,
      fun(I, Key, Acc) ->
    	      case {read_value(?TAB, Key, ?USER_DB_ROLE_AID),
    		    read_value(?TAB, Key, ?USER_DB_ROLE_RID)} of
		  {false,_} -> Acc;
		  {_,false} -> Acc;
		  {AID, Role} ->
    		      [{I, exodm_db:decode_id(Role)} | Acc];
                  {_OtherAID, _} -> Acc
    	      end
      end, [], exodm_db:join_key(UID, ?USER_ROLE)).

%%--------------------------------------------------------------------
%%
%% Utils
%%
%%--------------------------------------------------------------------
insert_password(Key, Item, Value) ->
    ?debug("key ~p, item ~p, value ~p ", [Key, Item, Value]),
    {ok, Salt} = bcrypt:gen_salt(),
    %% Expensive hash; expensive to create, expensive to check against
    %% So make sure we can not use this fact for a DOS attack!
    {ok, Hash} = bcrypt:hashpw(Value, Salt),
    ?debug("hash ~p", [Hash]),
    insert(Key, Item, to_binary(Hash)).

new_list_pos(Base) ->
    {ok, Last} = kvdb_conf:last_list_pos(?TAB, Base),
    Last+1.

insert(Key, Item, Value) when is_binary(Item) ->
    Key1 = exodm_db:join_key([Key, Item]),
    exodm_db:write(?TAB, Key1, Value).

delete(Key, Item) when is_binary(Item) ->
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
