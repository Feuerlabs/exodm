%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense device manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_group).

-export([init/1]).
-export([new/2,
	 update/3,
	 lookup/2, lookup/3,
	 exist/2,
	 delete/2,
	 add_members_to_groups/3,
	 remove_members_from_groups/3,
	 add_device/3,
	 remove_device/3,
	 device_is_deleted/3,
	 list_devices/2, list_devices/4,
	 table/1]).
-export([list_group_keys/2,
	 list_group_keys/3,
	 list_groups/3]).
-import(exodm_db, [write/2, binary_opt/2, to_binary/1]).

-include_lib("lager/include/log.hrl").

%%
%% /<aid>/groups/<gid>/name  = <DeviceName>
%% /<aid>/groups/<gid>/url   = <MSISDN>
%%

init(AID) ->
    exodm_db:in_transaction(
      fun(_) ->
	      Tab = table(AID),
	      kvdb_conf:add_table(Tab, []),
	      exodm_db:write(Tab, <<"__last_gid">>, <<0:32>>)
      end).

table(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    <<AID/binary, "_grp">>.

%% FIXME option validation
new(AID0, Options) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:in_transaction(
      fun(_) ->
	      new(AID, new_group_id(AID), Options)
      end).

new_group_id(AID) ->
    exodm_db:update_counter(table(AID), <<"__last_gid">>, 1).

new(AID, GID0, Options) ->
    Tab = table(AID),
    GID = exodm_db:group_id_key(GID0),
    case read(Tab, GID, name) of
	[] ->
	    Name = binary_opt(name, Options),
	    insert(Tab, GID, name,     Name),
	    insert(Tab, GID, url,      binary_opt(url,  Options)),
	    {ok, gid_value(GID)};
	[_] ->
	    {error, exists}
    end.

%% FIXME validate every item BEFORE insert!
update(AID0, GID0, Options) ->
    AID = exodm_db:account_id_key(AID0),
    GID = exodm_db:group_id_key(GID0),
    exodm_db:in_transaction(
      fun(_) ->
	      update_(AID, GID, Options)
      end).

update_(AID, GID, Options) ->
    Tab = table(AID),
    case read(Tab, GID, name) of
	[] ->
	    {error, not_found};
	[{_, Name0}] ->
	    lists:foreach(
	      fun
		  ({name,Value}) ->
		      case to_binary(Value) of
			  Name0 -> ok;
			  NewName ->
			      insert(Tab,GID,name,NewName)
		      end;
		  ({url,Value}) ->
		      insert(Tab,GID,url,to_binary(Value))
	      end, Options)
    end.

delete(AID0, GID0) ->
    ?debug("~p:delete(~p, ~p)~n", [?MODULE, AID0, GID0]),
    AID = exodm_db:account_id_key(AID0),
    GID = exodm_db:group_id_key(GID0),
    exodm_db:in_transaction(
      fun(_) ->
	      delete_(AID, GID)
      end).

delete_(AID, GID) ->
    Tab = table(AID),
    case exist(AID, GID) of
	true ->
	    kvdb_conf:delete_all(Tab, GID);
	false ->
	    {error, not_found}
    end.

lookup(AID0, GID0) ->
    AID = exodm_db:account_id_key(AID0),
    GID = exodm_db:group_id_key(GID0),
    Tab = table(AID),
    exodm_db:in_transaction(
      fun(_) ->
	      [{id,gid_value(GID)}] ++
		  read(Tab, GID, name) ++
		  read(Tab, GID, url)
      end).

lookup(AID0, GID0, Attr) when Attr==name; Attr==url ->
    AID = exodm_db:account_id_key(AID0),
    GID = exodm_db:group_id_key(GID0),
    Tab = table(AID),
    read(Tab, GID, Attr).

exist(AID, GID0) ->
    Tab = table(AID),
    GID = exodm_db:group_id_key(GID0),
    case read(Tab,GID,name) of
	[] -> false;
	[_] -> true
    end.

list_group_keys(AID, Limit) ->
    list_group_keys(AID, Limit, 0).

list_group_keys(AID, Limit, Prev0) when is_integer(Limit), Limit > 0 ->
    %% The __last_gid is a non-group object in this table, and will always
    %% lie before all group IDs (which are of the form "g........").
    Prev = erlang:max(exodm_db:group_id_key(Prev0), <<"__last_gid">>),
    exodm_db:list_next(table(AID), Limit, Prev, fun(K) -> K end).

list_groups(AID, Limit, Prev0) when is_integer(Limit), Limit > 0 ->
        %% The __last_gid is a non-group object in this table, and will always
    %% lie before all group IDs (which are of the form "g........").
    Prev = erlang:max(exodm_db:group_id_key(Prev0), <<"__last_gid">>),
    exodm_db:list_next(table(AID), Limit, Prev,
		       fun(Key) ->
			       [Grp] = kvdb_conf:split_key(Key),
			       exodm_db_group:lookup(AID, Grp)
		       end).

add_members_to_groups(AID0, GIDs, DIDs) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:in_transaction(
      fun(_) ->
	      lists:foreach(
		fun(GID) ->
			lists:foreach(
			  fun(DID) ->
				  add_device(AID, GID, DID)
			  end, DIDs)
		end, GIDs)
      end).

remove_members_from_groups(AID0, GIDs, DIDs) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:in_transaction(
      fun(_) ->
	      lists:foreach(
		fun(GID) ->
			lists:foreach(
			  fun(DID) ->
				  remove_device(AID, GID, DID)
			  end, DIDs)
		end, GIDs)
      end).

add_device(AID0, GID0, DID) ->
    ?debug("~p:add_device(~p, ~p, ~p)~n", [?MODULE, AID0, GID0, DID]),
    {Tab, GID} = tab_and_gid(AID0, GID0),
    Key = exodm_db:join_key([GID, <<"devices">>, DID]),
    kvdb_conf:write(Tab, {Key, [], <<>>}),
    exodm_db_device:do_add_group(AID0, DID, GID).

remove_device(AID0, GID0, DID) ->
    {Tab, GID} = tab_and_gid(AID0, GID0),
    Key = exodm_db:join_key([GID, <<"devices">>, DID]),
    kvdb_conf:delete(Tab, Key),
    exodm_db_device:do_remove_group(AID0, DID, GID).

device_is_deleted(AID0, DID0, GIDs) ->
    AID = exodm_db:account_id_key(AID0),
    Tab = table(AID),
    DID = exodm_db:encode_id(DID0),
    exodm_db:in_transaction(
      fun(_) ->
	      lists:foreach(
		fun(GID) ->
			remove_device(AID, GID, DID)
		end, GIDs)
      end).

list_devices(AID, GID) ->
    list_devices(AID, GID, 30, <<>>).

list_devices(AID0, GID0, Limit, Prev0) when is_integer(Limit), Limit > 0 ->
    {Tab, GID} = tab_and_gid(AID0, GID0),
    F = fun(K) -> lists:last(kvdb_conf:split_key(K)) end,
    case to_binary(Prev0) of
	<<>> ->
	    exodm_db:n_children(
	      Tab, Limit, exodm_db:join_key(GID, <<"devices">>), F);
	PrevDevice ->
	    Prev = exodm_db:join_key([GID, <<"devices">>, PrevDevice]),
	    exodm_db:list_next(Tab, Limit, Prev, F)
    end.

%% utils

insert(Tab, GID, Item, Value) ->
    Key = exodm_db:join_key(GID, to_binary(Item)),
    exodm_db:write(Tab, Key, Value).

tab_and_gid(AID0, GID0) ->
    Tab = table(exodm_db:account_id_key(AID0)),
    GID = exodm_db:group_id_key(GID0),
    {Tab, GID}.

read(Tab, GID,Item) ->
    Key = exodm_db:join_key(GID, to_binary(Item)),
    case exodm_db:read(Tab, Key) of
	{ok,{_,_,Value}} -> [{Item,Value}];
	{error,not_found} -> []
    end.

gid_value(GID) ->
    <<(exodm_db:group_id_num(GID)):32>>.
