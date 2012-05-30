%%% @author Tony Rogvall <tony@rogvall.se>
%%% @author Ulf Wiger <ulf@feuerlabs.com>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense user manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_user).

-export([new/3, update/2, lookup/1, lookup_attr/2,
	 list_access/1, exist/1]).

-import(exodm_db, [write/2, binary_opt/2, to_binary/1]).

%%
%% /user/<uname>/'__uid'    = User ID
%% /user/<uname>/'__aid'    = Account ID
%% /user/<uname>/name       = ShortName
%% /user/<uname>/fullname   = Name|Company
%% /user/<uname>/phone      = Phone number to contact
%% /user/<uname>/email      = Email to contact
%% /user/<uname>/skype      = Skype ID of contact
%% /user/<uname>/__password = password - stored encrypted!
%% /user/<uname>/access[<u>]/__aid  = Device owner ID
%% /user/<uname>/access[<u>]/__gid  = Device group ID
%% /user/<uname>/access[<u>]/__perm = Device group access
%%
new(AID, UName, Options) ->
    UID = new_uid(),
    [_] = exodm_db:nc_key_split(UName),  %% validation!
    Key = key(UName),
    insert(Key,name,UName),
    insert(Key,'__uid', exodm_db:user_id_key(UID)),
    insert(Key,'__aid', exodm_db:account_id_key(AID)),
    exodm_db_system:set_uid_user(UID, UName),
    insert(Key,fullname,      binary_opt(fullname,Options)),
    insert(Key,phone,         binary_opt(phone,Options)),
    insert(Key,email,         binary_opt(email,Options)),
    insert(Key,skype,         binary_opt(skype,Options)),
    insert_password(Key, '__password', binary_opt('__password', Options)),
    lists:foreach(
      fun({I,AAID,AGID,Perm}) ->
	      insert_access(Key, I, AAID, AGID, Perm)
      end, proplists:get_all_values(access, Options)),
    {ok, UID}.


new_uid() ->
    exodm_db_system:new_uid().

update(UName, Options) ->
    Key = key(UName),
    Ops =
	lists:map(
	  fun
	      ({fullname,Value}) ->
		  fun() -> insert(Key,fullname, to_binary(Value)) end;
	      ({phone,Value}) ->
		  fun() -> insert(Key,phone,    to_binary(Value)) end;
	      ({email,Value}) ->
		  fun() -> insert(Key,email,    to_binary(Value)) end;
	      ({skype,Value}) ->
		  fun() -> insert(Key,skype,    to_binary(Value)) end;
	      ({account,Acct}) ->
		  Account = to_binary(Acct),
		  case exodm_db_system:get_aid_user(Account) of
		      {error, not_found} ->
			  erlang:error({no_such_aid, Account});
		      _ ->
			  fun() -> insert(Key, account, Account) end
		  end;
	      ({'__password',Value}) ->
		  fun() ->
			  insert_password(Key, '__password', to_binary(Value))
		  end;
	      ({access, {I,AUID,AGID,Perm}}) ->
		  fun() -> insert_access(Key, I, AUID, AGID, Perm) end
	  end, Options),
    lists:foreach(fun(Op) -> Op() end, Ops).

lookup(UName) ->
    lookup_(key(UName)).

lookup_(Key) ->
    read(Key,name) ++
	read(Key,fullname) ++
	read(Key,phone) ++
	read(Key,email) ++
	read(Key,skype).

lookup_attr(UName, Attr) ->
    Key = key(UName),
    read(Key, Attr).

exist(UName) ->
    exist_(key(UName)).

exist_(Key) ->
    case read(Key,name) of
	[] -> false;
	[_] -> true
    end.

key(UName) ->
    exodm_db:kvdb_key_join(<<"user">>, UName).


insert(Key, Item, Value) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    exodm_db:write(Key1, Value).

insert_access(K0, I, AID0, GID0, Perm) when 
      is_integer(I), I>=0,
      (Perm =:= r orelse Perm =:= w orelse Perm =:= rw ) ->
    {AID, GID} = case {AID0, GID0} of
		     _ when is_integer(AID0), AID0 >= 0,
			    is_integer(GID0), GID0 >= 0 ->
			 {<<AID0:32>>, <<GID0:32>>};
		     {<<$a,_/binary>>, <<$g,_/binary>>} = Bins ->
			 Bins;
		     {<<_:32>>, <<_:32>>} = Bins ->
			 Bins;
		     Other ->
			 erlang:error({bad_type, Other})
		 end,
    K = exodm_db:kvdb_key_join(K0, exodm_db:list_key(access, I)),
    insert(K, '__aid', exodm_db:account_id_key(AID)),
    insert(K, '__gid', exodm_db:group_id_key(GID)),
    insert(K, '__perm', to_binary(Perm)).

list_access(UName) ->
    Key = exodm_db:kvdb_key_join([key(UName), <<"access">>]),
    group_access(Key, pfx_match_(
			kvdb:prefix_match(kvdb_conf,<<"data">>,Key, infinity))).

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


insert_password(Key, Item, Value) ->
    {ok, Salt} = bcrypt:gen_salt(),
    %% Expensive hash; expensive to create, expensive to check against
    {ok, Hash} = bcrypt:hashpw(Value, Salt),
    insert(Key, Item, to_binary(Hash)).

read(Key,Item) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    case exodm_db:read(Key1) of
	{ok,{_,_,Value}} -> 
	    [{Item,Value}];
	{error,not_found} -> []
    end.

