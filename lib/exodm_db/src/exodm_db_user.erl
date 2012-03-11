%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense user manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_user).

-export([new/3, update/3, lookup/2, lookup/1, exist/2, exist/1]).

-import(exodm_db, [write/2, binary_opt/2, to_binary/1]).

%%
%% /user/<uname>/name       = ShortName
%% /user/<uname>/fullname   = Name|Company
%% /user/<uname>/phone      = Phone number to contact
%% /user/<uname>/email      = Email to contact
%% /user/<uname>/skype      = Skype ID of contact
%% /user/<uname>/__password = password - FIXME -store encrypted!
%% /user/<uname>/access[<u>]/__uid  = Device owner ID
%% /user/<uname>/access[<u>]/__gid  = Device group ID
%% /user/<uname>/access[<u>]/__perm = Device group access
%%
new(UID, UName, Options) ->
    [_] = exodm_db:nc_key_split(UName),  %% validation!
    Key = key(UID, UName),
    insert(Key,name,UName),
    insert(Key,fullname,      binary_opt(fullname,Options)),
    insert(Key,phone,         binary_opt(phone,Options)),
    insert(Key,email,         binary_opt(email,Options)),
    insert(Key,skype,         binary_opt(skype,Options)),
    insert_password(Key,'__password',  binary_opt('__password',Options)),
    lists:foreach(
      fun({I,AUID,AGID,Perm}) ->
	      insert_access(Key, I, AUID, AGID, Perm)
      end, proplists:get_all_values(access, Options)),
    ok.

update(UID, UName, Options) ->
    Key = key(UID, UName),
    lists:foreach(
      fun
	  ({fullname,Value}) ->
	      insert(Key,fullname, to_binary(Value));
	  ({phone,Value}) ->
	      insert(Key,phone,    to_binary(Value));
	  ({email,Value}) ->
	      insert(Key,email,    to_binary(Value));
	  ({skype,Value}) ->
	      insert(Key,skype,    to_binary(Value));
	  ({'__password',Value}) ->
	      insert_password(Key,'__password', to_binary(Value));
	  ({access, {I,AUID,AGID,Perm}}) ->
	      insert_access(Key, I, AUID, AGID, Perm)
      end, Options).

lookup(UID, UName) ->
    lookup(key(UID, UName)).

lookup(Key) ->
    read(Key,name) ++
	read(Key,fullname) ++
	read(Key,phone) ++
	read(Key,email) ++
	read(Key,skype).

exist(UID, UName) ->
    exist(key(UID,UName)).

exist(Key) ->
    case read(Key,name) of
	[] -> false;
	[_] -> true
    end.

key(_UID, UName) ->
    exodm_db:kvdb_key_join(<<"user">>, UName).


insert(Key, Item, Value) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    exodm_db:write(Key1, Value).

insert_access(K0, I, UID, GID, Perm) when 
      is_integer(I), I>=0, 
      is_integer(UID), UID >= 0,
      is_integer(GID), GID >= 0,
      (Perm =:= r orelse Perm =:= w orelse Perm =:= rw ) ->
    K = exodm_db:kvdb_key_join(K0, exodm_db:list_key(access, I)),
    insert(K, '__uid', <<UID:32>>),
    insert(K, '__gid', <<GID:32>>),
    insert(K, '__perm', to_binary(Perm)).

insert_password(Key, Item, Value) ->
    %% FIXME encrypt before insert
    insert(Key, Item, Value).

read(Key,Item) ->
    Key1 = exodm_db:kvdb_key_join([Key, to_binary(Item)]),
    case exodm_db:read(Key1) of
	{ok,{_,_,Value}} -> 
	    [{Item,Value}];
	{error,not_found} -> []
    end.

