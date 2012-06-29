%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%% @author Ulf Wiger <ulf@feuerlabs.com>
%% @copyright 2012 Feuerlabs, Inc.
-module(exodm_db_yang).

-export([read/1, read/2,
	 write/2, write/3,
	 delete/1, delete/2,
	 find/2, find/3,
	 rpcs/1, rpcs/2]).
-export([init/0, init/1]).

-define(DB, kvdb_conf).

init() ->
    add_table(tab_name(system)),
    add_table(tab_name(shared)).

init(AID) ->
    add_table(tab_name(AID)).

add_table(Tab) ->
    kvdb:add_table(?DB, Tab, [{encoding, {raw,sext,raw}},
                              {index, ix_attrs()}]).


tab_name(system) ->
    <<"system_yang">>;
tab_name(shared) ->
    <<"shared_yang">>;
tab_name(AID) ->
    exodm_db:table(exodm_db:account_id_key(AID), <<"yang">>).

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

read(AID0, Y) ->
    AID = exodm_db:account_id_key(AID0),
    yang_parser:parse(Y, [{open_hook, fun(F, Opts) ->
					      open_file_hook(AID, F, Opts)
				      end}]).

write(File, YangSpec) ->
    write(get_aid(), File, YangSpec).

write(AID0, File, Y) ->
    AID = exodm_db:account_id_key(AID0),
    Opts = [{open_hook, fun(F, Os) when F == File ->
				open_bin_hook(AID, F, [{data,Y}|Os]);
			   (F, Os) ->
				open_file_hook(AID, F, Os)
			end}],
    case yang_parser:parse(File, Opts) of
	{ok, [Module]} ->
	    RPCs = case yang_json:json_rpc(File, Opts) of
		       [{module,_,RPCs1}] -> RPCs1;
		       _ -> []
		   end,
	    store(AID, File, Module, RPCs, Y);
	{error,_} = Error ->
	    Error
    end.

delete(File) ->
    delete(get_aid(), File).

delete(AID, File) ->
    kvdb:delete(?DB, tab_name(AID), File).

rpcs(File) ->
    rpcs(get_aid(), File).

rpcs(AID, File) ->
    kvdb:get_attrs(?DB, tab_name(AID), file_key(File), [rpcs]).

store(AID, File, {module, _, M, L} = Mod, RPCs, Src) ->
    Checksum = compute_checksum(Mod),
    io:fwrite("Specs with same checksum: ~p~n",
	      [find(AID, '__checksum', Checksum)]),
    kvdb:put(?DB, tab_name(AID),
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

find(AID, Ix, V) ->
    kvdb:index_keys(?DB, tab_name(AID), Ix, V).

open_file_hook(AID, File, Opts) ->
    FBin = to_binary(filename:basename(File)),
    try case kvdb:get(?DB, tab_name(AID), FBin) of
	    {ok, {_, _, Bin}} ->
		open_bin_hook(AID, File, [{data,Bin}|Opts]);
	    {error, _} ->
                case kvdb:get(?DB, tab_name(system), FBin) of
                    {ok, {_, _, Bin}} ->
                        open_bin_hook(system, File, [{data,Bin}|Opts]);
                    {error,_} ->
                        {error, enoent}
                end
	end
    catch
	error:_ ->
	    {error, einval}
    end.

open_bin_hook(AID0, File, Opts) ->
    AID = exodm_db:account_id_key(AID0),
    Bin = proplists:get_value(data, Opts, <<>>),
    case file:open(filename:join(AID, File), [read, write, ram, binary]) of
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
