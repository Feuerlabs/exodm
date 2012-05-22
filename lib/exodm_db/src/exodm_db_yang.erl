%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%% @author Ulf Wiger <ulf@feuerlabs.com>
%% @copyright 2012 Feuerlabs, Inc.
-module(exodm_db_yang).

-export([read/1, write/2, delete/1, find/2]).
-export([init/0]).

-define(DB, kvdb_conf).

init() ->
    kvdb:add_table(?DB, yang, [{encoding, {raw,sext,raw}},
			       {index, ix_attrs()}]).

ix_attrs() ->
    [{K, ix_type(K)} || K <- [module, grouping, namespace,
			      container, typedef, rpc]].
ix_type(module   ) -> value;
ix_type(grouping ) -> each;
ix_type(namespace) -> each;
ix_type(container) -> each;
ix_type(typedef  ) -> each;
ix_type(rpc      ) -> each;
ix_type(_) -> undefined.

attrs(M, L) ->
    attrs_(L, orddict:from_list([{module,M}])).

attrs_([{K,_,Nm,L}|T], Acc) ->
    Acc1 = case ix_type(K) of
	       each -> orddict:append(K, Nm, Acc);
	       value -> orddict:store(K, Nm, Acc);
	       undefined -> Acc
	   end,
    attrs_(T, attrs_(L, Acc1));
attrs_([], Acc) ->
    Acc.

read(Y) ->
    yang_parser:parse(Y, [{open_hook, fun open_file_hook/2}]).

write(File, Y) ->
    case yang_parser:parse(
	   File, [{open_hook, fun(F, Opts) ->
				      open_bin_hook(F, [{data,Y}|Opts])
			      end}]) of
	{ok, [Module]} ->
	    store(File, Module, Y);
	{error,_} = Error ->
	    Error
    end.

delete(File) ->
    kvdb:delete(?DB, yang, File).

store(File, {module, _, M, L}, Src) ->
    kvdb:put(?DB, yang, {to_binary(File), attrs(M, L), to_binary(Src)}).

find(Ix, V) ->
    kvdb:index_keys(?DB, yang, Ix, V).

open_file_hook(File, Opts) ->
    try case kvdb:get(?DB, yang, to_binary(File)) of
	    {ok, {_, _, Bin}} ->
		open_bin_hook(File, [{data,Bin}|Opts]);
	    {error, _} ->
		{error, enoent}
	end
    catch
	error:_ ->
	    {error, einval}
    end.

open_bin_hook(File, Opts) ->
    Bin = proplists:get_value(data, Opts, <<>>),
    case file:open(File, [read, write, ram, binary]) of
	{ok, Fd} ->
	    ok = file:write(Fd, to_binary(Bin)),
	    {ok,0} = file:position(Fd,bof),
	    {ok, Fd};
	{error, _} ->
	    {error, enoent}
    end.

to_binary(X) ->
    exodm_db:to_binary(X).
