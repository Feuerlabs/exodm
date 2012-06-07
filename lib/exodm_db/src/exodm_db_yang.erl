%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%% @author Ulf Wiger <ulf@feuerlabs.com>
%% @copyright 2012 Feuerlabs, Inc.
-module(exodm_db_yang).

-export([read/1, read/2,
	 write/2, write/3,
	 delete/1, delete/2,
	 find/2, find/3,
	 rpcs/1, rpcs/2]).
-export([init/1]).

-define(DB, kvdb_conf).

init(AID) ->
    kvdb:add_table(?DB, tab_name(AID),
		   [{encoding, {raw,sext,raw}},
		    {index, ix_attrs()}]).

tab_name(AID) ->
    exodm_db:table(AID, <<"yang">>).

ix_attrs() ->
    [{K, ix_type(K)} || K <- [module, revision, grouping, namespace, uses,
			      container, typedef, rpc, '__checksum']].
ix_type(module   ) -> value;
ix_type(revision ) -> value;
ix_type('__checksum') -> value;
ix_type(grouping ) -> each;
ix_type(uses     ) -> each;
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

read(Y) -> read(get_aid(), Y).

read(User, Y) ->
    yang_parser:parse(Y, [{open_hook, fun(F, Opts) ->
					      open_file_hook(User, F, Opts)
				      end}]).

write(File, YangSpec) ->
    write(get_aid(), File, YangSpec).

write(User, File, Y) ->
    Opts = [{open_hook, fun(F, Os) when F == File ->
				open_bin_hook(User, F, [{data,Y}|Os]);
			   (F, Os) ->
				open_file_hook(User, F, Os)
			end}],
    case yang_parser:parse(File, Opts) of
	{ok, [Module]} ->
	    RPCs = case yang_json:json_rpc(File, Opts) of
		       [{module,_,RPCs1}] -> RPCs1;
		       _ -> []
		   end,
	    store(User, File, Module, RPCs, Y);
	{error,_} = Error ->
	    Error
    end.

delete(File) ->
    delete(get_aid(), File).

delete(User, File) ->
    kvdb:delete(?DB, tab_name(User), File).

rpcs(File) ->
    rpcs(get_aid(), File).

rpcs(User, File) ->
    kvdb:get_attrs(?DB, tab_name(User), file_key(File), [rpcs]).

store(User, File, {module, _, M, L} = Mod, RPCs, Src) ->
    Checksum = compute_checksum(Mod),
    io:fwrite("Specs with same checksum: ~p~n",
	      [find(User, '__checksum', Checksum)]),
    kvdb:put(?DB, tab_name(User),
	     {file_key(File),
	      [{'__checksum', Checksum},
	       {rpcs, RPCs}
	       |attrs(M, L)],
	      to_binary(Src)}).

file_key(File) when is_binary(File) ->
    File;
file_key(File) ->
    to_binary(filename:basename(File)).

compute_checksum(Term) ->
    erlang:md5(term_to_binary(Term)).


find(Ix, V) ->
    find(get_aid(), Ix, V).

find(User, Ix, V) ->
    kvdb:index_keys(?DB, tab_name(User), Ix, V).

open_file_hook(User, File, Opts) ->
    try case kvdb:get(?DB, tab_name(User),
		      to_binary(filename:basename(File))) of
	    {ok, {_, _, Bin}} ->
		open_bin_hook(User, File, [{data,Bin}|Opts]);
	    {error, _} ->
		{error, enoent}
	end
    catch
	error:_ ->
	    {error, einval}
    end.

open_bin_hook(User, File, Opts) ->
    Bin = proplists:get_value(data, Opts, <<>>),
    case file:open(filename:join(User, File), [read, write, ram, binary]) of
	{ok, Fd} ->
	    ok = file:write(Fd, to_binary(Bin)),
	    {ok,0} = file:position(Fd,bof),
	    {ok, Fd};
	{error, _} ->
	    {error, enoent}
    end.

to_binary(X) ->
    exodm_db:to_binary(X).

get_aid() ->
    exodm_db_session:get_aid().
