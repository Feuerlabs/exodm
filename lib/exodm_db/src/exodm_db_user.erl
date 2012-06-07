%%% @author Tony Rogvall <tony@rogvall.se>
%%% @author Ulf Wiger <ulf@feuerlabs.com>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense user manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_user).

-export([init/0, init/1]).
-export([new/3, update/2, lookup/1, lookup/2, lookup_attr/2,
	 list_access/1, list_access/2, exist/1, exist/2]).
-export([ix_alias/1]).
-import(exodm_db, [write/2, binary_opt/2, binary_opt/3, to_binary/1]).

-record(user, {aid, uid}).

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
    kvdb_conf:add_table(<<"user">>, [{index, [{alias, each,
					       {?MODULE, ix_alias}}]}]).

init(AID) ->
    kvdb_conf:add_table(table(AID), []).

table(AID) ->
    exodm_db:table(AID, <<"user">>).


new(AID0, UID, Options) ->
    [_] = exodm_db:nc_key_split(UID),  %% validation!
    Key = exodm_db:user_id_key(UID),
    IUID = exodm_db_system:new_uid(),
    AID = exodm_db:account_id_key(AID0),
    UTab = <<"user">>,
    ATab = table(AID),
    Name = binary_opt(name, Options, UID),
    case exist_(UTab, Key) of
	true ->
	    {error, exists};
	false ->
	    insert(ATab, Key,name, UID),
	    %%
	    insert(UTab, Key,name, Name),
	    insert(UTab, Key,'__uid', IUID),
	    insert(UTab, Key,'__aid', AID),
	    insert(UTab, Key,fullname,      binary_opt(fullname,Options)),
	    insert(UTab, Key,phone,         binary_opt(phone,Options)),
	    insert(UTab, Key,email,         binary_opt(email,Options)),
	    insert(UTab, Key,skype,         binary_opt(skype,Options)),
	    insert_password(UTab, Key, '__password',
			    binary_opt('__password', Options)),
	    lists:foreach(
	      fun({I,AAID,AGID,Perm}) ->
		      insert_access(UTab, Key, I, AAID, AGID, Perm)
	      end, proplists:get_all_values(access, Options)),
	    {ok, UID}
    end.

ix_alias({K, _, _V}) ->
    case exodm_db:kvdb_key_split(K) of
	Split ->
	    io:fwrite("Split = ~p~n", [Split]);
	_ ->
	    ok
    end,
    [].

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
	      ({access, {I,AUID,AGID,Perm}}) ->
		  fun() -> insert_access(Tab, Key, I, AUID, AGID, Perm) end
	  end, Options),
    lists:foreach(fun(Op) -> Op() end, Ops).

lookup(UID) ->
    lookup_(<<"user">>, key(UID)).

lookup(AID, UID) ->
    case exist(AID, UID) of
	true ->
	    lookup(UID);
	false ->
	    []
    end.

lookup_(Tab, Key) ->
    read(Tab, Key,name) ++
	read(Tab, Key,fullname) ++
	read(Tab, Key,phone) ++
	read(Tab, Key,email) ++
	read(Tab, Key,skype).

lookup_attr(UName, Attr) ->
    Tab = <<"user">>,
    Key = exodm_db:user_id_key(UName),
    read(Tab, Key, Attr).

exist(UID) ->
    Tab = <<"user">>,
    exist_(Tab, exodm_db:user_id_key(UID)).

exist(AID, UID) ->
    Tab = table(AID),
    exist_(Tab, exodm_db:user_id_key(UID)).

exist_(Tab, Key) ->
    case read(Tab, Key,name) of
	[] -> false;
	[_] -> true
    end.

key(UID) ->
    exodm_db:user_id_key(UID).

insert(Tab, Key, Item, Value) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    exodm_db:write(Tab, Key1, Value).

insert_access(Tab, K0, I, AID0, GID0, Perm) when 
      is_integer(I), I>=0,
      (Perm =:= r orelse Perm =:= w orelse Perm =:= rw ) ->
    AID = exodm_db:account_id_key(AID0),
    GID = exodm_db:group_id_key(GID0),
    %% {AID, GID} = case {AID0, GID0} of
    %% 		     _ when is_integer(AID0), AID0 >= 0,
    %% 			    is_integer(GID0), GID0 >= 0 ->
    %% 			 {<<AID0:32>>, <<GID0:32>>};
    %% 		     {<<$a,_/binary>>, <<$g,_/binary>>} = Bins ->
    %% 			 Bins;
    %% 		     {<<_:32>>, <<_:32>>} = Bins ->
    %% 			 Bins;
    %% 		     Other ->
    %% 			 erlang:error({bad_type, Other})
    %% 		 end,
    K = exodm_db:kvdb_key_join(K0, exodm_db:list_key(access, I)),
    insert(Tab, K, '__aid', exodm_db:account_id_key(AID)),
    insert(Tab, K, '__gid', exodm_db:group_id_key(GID)),
    insert(Tab, K, '__perm', to_binary(Perm)).

list_access(UID0) when is_binary(UID0) ->
    UID = exodm_db:user_id_key(UID0),
    case read(<<"user">>, UID, '__aid') of
	[] -> [];
	[{_, AID}] ->
	    list_access(AID, UID)
    end;
list_access(#user{aid = AID, uid = UID}) ->
    list_access(AID, UID).

list_access(AID, UID0) ->
    Tab = table(AID),
    UID = exodm_db:user_id_key(UID0),
    Key = exodm_db:kvdb_key_join(UID, <<"access">>),
    group_access(Key, pfx_match_(
			kvdb:prefix_match(kvdb_conf,Tab,Key, infinity))).

group_access(K, A) ->
    Sz = byte_size(K),
    Split = [{exodm_db:kvdb_key_split(Rest), Data} ||
		{<<K1:Sz/binary, Rest/binary>>,[],Data} <- A, K=:=K1],
    lists:foldl(fun({[X,Y], Z}, Acc) ->
			orddict:append(x_to_int(X),
				       {binary_to_atom(Y, latin1),Z}, Acc)
		end, orddict:new(), Split).

x_to_int(Bin) ->
    {match, [S]} = re:run(Bin,<<"[0-9]+">>, [{capture,first,list}]),
    list_to_integer(S).

pfx_match_({L, Cont}) ->
    case Cont() of
	done -> L;
	Other -> pfx_match_(Other)
    end;
pfx_match_(done) ->
    [].


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

