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
	      kvdb_conf:add_table(Tab, [])
      end).

table(AID0) ->
    AID = exodm_db:account_id_key(AID0),
    <<AID/binary, "_grp">>.

%% FIXME option validation
new(AID0, Options) ->
    AID = exodm_db:account_id_key(AID0),
    case binary_opt(name, Options) of
	<<>> -> {error, {required, name}};
	Name ->
	    exodm_db:in_transaction(
	      fun(_) ->
		      new(AID, Name, Options)
	      end)
    end.

new(AID, Name, Options) ->
    {Tab, GID} = tab_and_gid(AID, Name),
    case kvdb_conf:read(Tab, GID) of
	{error, not_found} ->
	    kvdb_conf:write(Tab, {GID, [], <<>>}),
	    kvdb_conf:write(
	      Tab, {kvdb_conf:join_key(GID, <<"url">>), [],
		    binary_opt(url,  Options)}),
	    ok;
	[_] ->
	    {error, exists}
    end.

%% FIXME validate every item BEFORE insert!
update(AID0, Name, Options) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:in_transaction(
      fun(_) ->
	      update_(AID, Name, Options)
      end).

update_(AID, Name, Options) ->
    {Tab, GID} = tab_and_gid(AID, Name),
    case kvdb_conf:read(Tab, GID) of
	{error, not_found} ->
	    {error, not_found};
	{ok, _} ->
	    lists:foreach(
	      fun
		  ({Key,Value}) ->
		      insert(Tab,GID,to_binary(Key),to_binary(Value))
	      end, Options)
    end.

delete(AID, Name) ->
    ?debug("~p:delete(~p, ~p)~n", [?MODULE, AID, Name]),
    exodm_db:in_transaction(
      fun(_) ->
	      delete_(AID, Name)
      end).

delete_(AID, Name) ->
    {Tab, GID} = tab_and_gid(AID, Name),
    case kvdb_conf:read(Tab, GID) of
	{ok, _} ->
	    kvdb_conf:delete_all(Tab, GID);  %% delete_tree?
	{error, not_found} ->
	    {error, not_found}
    end.

lookup(AID, Name) ->
    {Tab, GID} = tab_and_gid(AID, Name),
    exodm_db:in_transaction(
      fun(_) ->
	      case kvdb_conf:read(Tab, GID) of
		  {ok, _} ->
		      Dec = exodm_db:decode_id(GID),
		      [{'group-id', Dec}] ++ read(Tab, GID, url);
		  {error, not_found} ->
		      []
	      end
      end).

lookup(AID, Name, Attr) when Attr==name; Attr==url ->
    {Tab, GID} = tab_and_gid(AID, Name),
    read(Tab, GID, Attr).

exist(AID, Name) ->
    {Tab, GID} = tab_and_gid(AID, Name),
    case kvdb_conf:read(Tab, GID) of
	{error, not_found} -> false;
	{ok, _} -> true
    end.

list_group_keys(AID, Limit) ->
    list_group_keys(AID, Limit, 0).

list_group_keys(AID, Limit, Prev0) when is_integer(Limit), Limit > 0 ->
    {Tab, Prev} = tab_and_gid(AID, Prev0),
    exodm_db:list_next(Tab, Limit, Prev, fun(K) -> K end).

list_groups(AID, Limit, Prev0) when is_integer(Limit), Limit > 0 ->
    {Tab, Prev} = tab_and_gid(AID, Prev0),
    exodm_db:list_next(Tab, Limit, Prev,
		       fun(Key) ->
			       [Grp] = kvdb_conf:split_key(Key),
			       exodm_db:read_all(Tab, Grp)
		       end).

add_members_to_groups(AID0, Groups, DIDs) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:in_transaction(
      fun(_) ->
	      lists:foreach(
		fun(Grp) ->
			lists:foreach(
			  fun(DID) ->
				  add_device(AID, Grp, DID)
			  end, DIDs)
		end, Groups)
      end).

remove_members_from_groups(AID0, Groups, DIDs) ->
    AID = exodm_db:account_id_key(AID0),
    exodm_db:in_transaction(
      fun(_) ->
	      lists:foreach(
		fun(Grp) ->
			lists:foreach(
			  fun(DID) ->
				  remove_device(AID, Grp, DID)
			  end, DIDs)
		end, Groups)
      end).

add_device(AID, Grp, DID) ->
    ?debug("~p:add_device(~p, ~p, ~p)~n", [?MODULE, AID, Grp, DID]),
    {Tab, GID} = tab_and_gid(AID, Grp),
    Key = exodm_db:join_key([GID, <<"devices">>, DID]),
    kvdb_conf:write(Tab, {Key, [], <<>>}),
    exodm_db_device:do_add_group(AID, DID, GID).

remove_device(AID, Grp, DID) ->
    {Tab, GID} = tab_and_gid(AID, Grp),
    Key = exodm_db:join_key([GID, <<"devices">>, DID]),
    kvdb_conf:delete(Tab, Key),
    exodm_db_device:do_remove_group(AID, DID, GID).

device_is_deleted(AID0, DID0, Groups) ->
    AID = exodm_db:account_id_key(AID0),
    DID = exodm_db:encode_id(DID0),
    exodm_db:in_transaction(
      fun(_) ->
	      lists:foreach(
		fun(GID) ->
			remove_device(AID, GID, DID)
		end, Groups)
      end).

list_devices(AID, GID) ->
    list_devices(AID, GID, 30, <<>>).

list_devices(AID, Grp, Limit, Prev0) when is_integer(Limit), Limit > 0 ->
    {Tab, GID} = tab_and_gid(AID, Grp),
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
    GID = exodm_db:encode_id(GID0),
    {Tab, GID}.

read(Tab, GID,Item) ->
    Key = exodm_db:join_key(GID, to_binary(Item)),
    case exodm_db:read(Tab, Key) of
	{ok,{_,_,Value}} -> [{Item,Value}];
	{error,not_found} -> []
    end.
