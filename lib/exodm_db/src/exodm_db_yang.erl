%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% @author Ulf Wiger <ulf@feuerlabs.com>
%% @copyright 2012 Feuerlabs, Inc.
-module(exodm_db_yang).

-export([read/1,     %% (YangF) -> read(get_aid(), YangF)
         read/2,     %% (AID, YangF) -> read(AID, YangF, [])
         read/3,     %% (AID, YangF, Opts)
	 write/2,    %% (YangF, YangSpec) -> write(get_aid(), YangF, YangSpec)
         write/3,    %% (AID, YangF, YangSpec)
         tag_file/2, %% (YangF, Tag) -> tag_file(AID, YangF, Tag)
         tag_file/3, %% (AID, YangF, Tag)
	 delete/1,   %% delete(YangF) -> delete(get_aid(), YangF)
         delete/2,   %% (AID, YangF)
	 find/2,     %% (Index, Value) -> find(get_aid(), Index, Value)
         find/3,     %% (AID, Index, Value)
	 rpcs/1,     %% (YangF) -> rpcs(get_aid(), YangF)
         rpcs/2,     %% (AID, YangF)
         find_rpc/2, %% (YangF, Method) -> find_rpc(get_aid(), YangF, Method)
         find_rpc/3, %% (AID, YangF, Method)
         specs/0,    %% () -> specs(get_aid())
         specs/1]).  %% (AID)
-export([init/0, init/1,
         write_system/2
        ]).
-export([table/1, list_next/3]).

-include_lib("lager/include/log.hrl").
-define(DB, kvdb_conf).

init() ->
    exodm_db:in_transaction(
      fun(_) ->
              add_table(tab_name(system)),
              add_table(tab_name(shared)),
              exodm_db_session:set_trusted_proc(),
              YangDir = filename:join(code:priv_dir(exosense_specs), "yang"),
              {ok, Bin1} = file:read_file(
                             filename:join(YangDir, "ietf-inet-types.yang")),
              _Res1 = write_system("ietf-inet-types.yang", Bin1),
              ?debug("write_system(ietf-inet-types.yang) -> ~p~n", [_Res1]),
              {ok, Bin2} = file:read_file(
                             filename:join(YangDir, "exosense.yang")),
              _Res2 = write_system("exosense.yang", Bin2),
              ?debug("write_system(exosense.yang) -> ~p~n", [_Res2]),
              {ok, Bin3} = file:read_file(
                             filename:join(YangDir, "exodm.yang")),
              _Res3 = write_system("exodm.yang", Bin3),
              ?debug("write_system(exodm.yang) -> ~p~n", [_Res3]),

              {ok, Bin4} = file:read_file(
                             filename:join(YangDir, "exodm_admin.yang")),
              _Res4 = write_system("exodm_admin.yang", Bin4),
              ?debug("write_system(exodm_admin.yang) -> ~p~n", [_Res4]),
              ok
      end).


init(AID) ->
    exodm_db:in_transaction(
      fun(_) ->
              add_table(tab_name(AID))
      end).

add_table(Tab) ->
    kvdb:add_table(?DB, Tab, [{encoding, {raw,sext,term}},
                              {index, ix_attrs()}]).

list_next(AID, N, Prev) ->
    Tab = table(AID),
    exodm_db:in_transaction(
      fun(_) ->
	      exodm_db:list_next(Tab,
				 N, exodm_db:to_binary(Prev),
				 fun(Key) ->
                                         Key
                                             %% lookup(Key)
				 end)
      end).

table(AID) ->
    tab_name(AID).

tab_name(system) ->
    <<"system_yang">>;
tab_name(shared) ->
    <<"shared_yang">>;
tab_name(AID) ->
    exodm_db:table(exodm_db:account_id_key(AID), <<"yang">>).

ix_attrs() ->
    [{K, ix_type(K)} || K <- [module, revision, grouping, namespace, uses,
			      container, typedef, rpc, '__checksum', exodm_tag]].
ix_type(module   ) -> value;
ix_type(revision ) -> value;
ix_type('__checksum') -> value;
ix_type(grouping ) -> each;
ix_type(uses     ) -> each;
ix_type(namespace) -> each;
ix_type(container) -> each;
ix_type(typedef  ) -> each;
ix_type(rpc      ) -> each;
ix_type(exodm_tag) -> each;
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
read(AID, Y) -> read(AID, Y, []).

read(AID0, Y, Opts0) ->
    {AID, _Key, _} = file_key(AID0, Y),
    yang_parser:parse(Y, [{open_hook, fun(F, Opts) ->
					      open_file_hook(AID, F, Opts)
				      end}|Opts0]).

write(File, YangSpec) ->
    write(get_aid(), File, YangSpec).

write(AID, File, Y) ->
    exodm_db:in_transaction(
      fun(_) ->
              write_(exodm_db:account_id_key(AID), File, Y)
      end).

tag_file(File, Tag) ->
    tag_file(get_aid(), File, Tag).

tag_file(AID0, File, Tag) ->
    {AID, _, _} = file_key(AID0, File),
    check_access(write, AID),
    exodm_db:in_transaction(
      fun(_) ->
              case try_read_file(AID, File, []) of
                  {ok, Tab, {K, Attrs, Bin}} ->
                      case orddict:find(exodm_tag, Attrs) of
                          {ok, OldTags} ->
                              case lists:member(Tag, OldTags) of
                                  true ->
                                      ok;
                                  false ->
                                      NewAttrs = orddict:store(
                                                   exodm_tag, [Tag|OldTags]),
                                      kvdb:put(?DB, Tab,
                                               {K, NewAttrs, Bin})
                              end;
                          error ->
                              NewAttrs = orddict:store(exodm_tag, Tag, Attrs),
                              kvdb:put(?DB, Tab, {K, NewAttrs, Bin})
                      end;
                  {error, _} = Err ->
                      Err
              end
      end).

write_system(File, Y) ->
    exodm_db:in_transaction(
      fun(_) ->
              write_(system, File, Y)
      end).

write_(AID0, File0, Y) ->
    {AID, _Key, _} = file_key(AID0, File0),
    check_access(write, AID),
    File = to_string(File0),
    Opts = [{open_hook, fun(F, Os) when F == File ->
				open_bin_hook(AID, F, [{data,Y}|Os]);
			   (F, Os) ->
				open_file_hook(AID, F, Os)
			end}],
    %% TODO: we should deep_parse the modules once that's ready
    case yang_parser:deep_parse(File, Opts) of
	{ok, [{module,_,_,Data} = Module]} ->
            RPCs = [D || D <- Data,
                         element(1, D) == rpc orelse
                             element(1, D) == notification],
	    %% RPCs = case yang_json:json_rpc(File, Opts) of
	    %%            [{module,_,RPCs1}] -> RPCs1;
            %%            {error, _} = Error ->
            %%                error(Error, [File])
	    %%        end,
	    store(AID, File, Module, RPCs, Y);
        {ok, [{submodule,_,_,_} = SubMod]} ->
            store(AID, File, SubMod, [], Y);
	{error,_} = Error ->
	    error(Error, [Y])
    end.

check_access(read, system) -> ok;
check_access(write, system) ->
    case exodm_db_session:is_trusted_proc() of
        true ->
            ok;
        false ->
            ?debug("is not trusted proc (~p)~n", [get()]),
            error(unauthorized)
    end;
check_access(_, AID) ->
    %% hm the AID here is normally? the session AID anyway...
    case exodm_db_session:get_aid() of
        root -> ok;
        SessAid ->
            case exodm_db:account_id_key(SessAid) of
                AID ->
                    ok;
                Other ->
                    error({unauthorized, [AID, Other]})
            end
    end.

%% delete yang module
%% the name should be something like
%% foo.yang             translated to foo@.yang (no revision)
%% foo@yyyy-mm-dd.yang
%% foo@.yang
delete(File) ->
    delete(get_aid(), File).

delete(AID0, File0) ->
    {AID, File1, _} = file_key(AID0, File0),
    Base = filename:basename(File1, <<".yang">>),
    Key = case binary:split(Base, <<"@">>) of
              [Name] -> <<Name/binary, "@.yang">>;
              _ -> <<Base/binary, ".yang">>
          end,
    check_access(write, AID),
    exodm_db:in_transaction(
      fun(_) ->
              kvdb_conf:delete_tree(table(AID), Key),
              kvdb:delete(?DB, tab_name(AID), Key)
      end).

rpcs(File) ->
    rpcs(get_aid(), File).

rpcs(AID0, File0) ->
    ?debug("rpcs(~p, ~p)~n", [AID0, File0]),
    {File, _Rev} = internal_filename(to_binary(File0)),
    {AID, Key, _} = file_key(AID0, File),
    check_access(read, AID),
    case kvdb:get_attrs(?DB, tab_name(AID), Key, [rpcs]) of
        {ok, Res} ->
            Res;
        {error,not_found} ->
            case filename:basename(File) of
                <<"system.", _/binary>> -> [];
                <<"user.", _/binary>>   -> [];
                _ ->
                    ?debug("trying system repository (~p)~n", [Key]),
                    rpcs(AID, <<"system.", Key/binary>>)
            end
    end.

find_rpc(File, Method) ->
    find_rpc(get_aid(), File, Method).

find_rpc(AID0, File0, Method) ->
    ?debug("find_rpc(~p, ~p, ~p)~n", [AID0, File0, Method]),
    {File, Rev} = internal_filename(to_binary(File0)),
    ?debug("internal file ~p, revision ~p~n", [File, Rev]),
    {AID, Key, IsTagged} = file_key(AID0, File),
    ?debug("aid ~p, key ~p, tagflag ~p~n", [AID, Key, IsTagged]),
    check_access(read, AID),
    ?debug("access checked~n", []),
    case Rev of
        <<>> ->
            %% no revision specified
            find_rpc_latest(AID, Key, Method, IsTagged);
        _ ->
            find_rpc_specified(AID, Key, Method, IsTagged)
    end.

find_rpc_latest(system, Key, Method, _) ->
    ?debug("find_rpc_latest(system, ~p, ~p)~n", [Key, Method]),
    case latest_file_version(Tab = tab_name(system), Key) of
        {ok, FullName} ->
            ?debug("latest file version ~p~n", [FullName]),
            lookup_rpc(Tab, FullName, Method);
        error ->
            {error, not_found}
    end;
find_rpc_latest(AID, Key, Method, IsTagged) ->
    ?debug("find_rpc_latest(~p, ~p, ~p, ~p)~n", [AID, Key, Method, IsTagged]),
    case latest_file_version(Tab = tab_name(AID), Key) of
        {ok, FullName} ->
            %% Given that we have the spec here, we only search in this spec.
            ?debug("latest file version ~p~n", [FullName]),
            lookup_rpc(Tab, FullName, Method);
        error ->
            if IsTagged ->
                    {error, not_found};
               true ->
                    find_rpc_latest(system, Key, Method, IsTagged)
            end
    end.

find_rpc_specified(system, Key, Method, _) ->
    lookup_rpc(tab_name(system), Key, Method);
find_rpc_specified(AID, Key, Method, IsTagged) ->
    case lookup_rpc(tab_name(AID), Key, Method) of
        {ok, _} = Found ->
            Found;
        {error, not_found} ->
            if IsTagged ->
                    {error, not_found};
               true ->
                    find_rpc_specified(system, Key, Method, IsTagged)
            end
    end.

lookup_rpc(Tab, FullName, Method) ->
    ?debug("lookup_rpc(~p, ~p, ~p)~n", [Tab, FullName, Method]),
    ShortMethod = case re:split(Method, <<":">>, [{return,binary}]) of
                      [_, M] -> M;
                      [M] -> M
                  end,
    Key = kvdb_conf:join_key([FullName, <<"rpc">>, to_binary(ShortMethod)]),
    ?debug("short method ~p, key ~p~n", [ShortMethod, Key]),
    kvdb_conf:read(Tab, Key).


specs() ->
    specs(get_aid()).

specs(AID) ->
    exodm_db:select(tab_name(AID), [{ {'$1','_','_'}, [], ['$1'] }]).

store(AID, File, {Tag, _, M, L} = Mod, RPCs, Src) when
      Tag==module; Tag==submodule ->
    Checksum = compute_checksum(Mod),
    io:fwrite("Specs with same checksum: ~p~n",
              [find(AID, '__checksum', Checksum)]),
    Revision = get_revision(L),
    {Tab, FName} = case to_binary(filename:basename(File)) of
                       <<"system.", Rest/binary>> ->
                           {tab_name(system), Rest};
                       <<"user.", Rest/binary>> ->
                           {tab_name(AID), Rest};
                       Other ->
                           {tab_name(AID), Other}
                   end,
    Key1 = set_revision(FName, Revision),
    kvdb:put(?DB, Tab, {kvdb_conf:escape_key(Key1),
                        [{'__checksum', Checksum}], <<>>}),
    kvdb:put(?DB, Tab, {kvdb_conf:join_key(Key1, <<"src">>), [],
                        to_binary(Src)}),
    %% ?debug("L = ~p\n", [L]),
    Attrs = attrs(M, L),
    %% ?debug("Attrs = ~p\n", [Attrs]),
    [kvdb:put(?DB, Tab, {kvdb_conf:join_key([Key1, <<"a">>, to_binary(A)]),
                         [], V}) || {A, V} <- Attrs],
    lists:foreach(
      fun({Stmt,_,R,_} = RPC) when Stmt==rpc; Stmt==notification ->
              kvdb:put(?DB, Tab, {kvdb_conf:join_key(
                                    [Key1, <<"rpc">>, to_binary(R)]), [], RPC})
      end, RPCs).


get_revision(L) ->
    case [R || {revision,_,R,_} <- L] of
        [] ->
            <<>>;
        [_|_] = Rs ->
            lists:max(Rs)
    end.

set_revision(F, Rev) ->
    Base = filename:basename(F, <<".yang">>),
    [Name|_] = binary:split(Base, <<"@">>),
    <<Name/binary, "@", Rev/binary, ".yang">>.

file_key(AID, File) ->
    case filename:basename(to_binary(File)) of
        <<"system.", Key/binary>> -> {system, Key, true};
        <<"user.", Key/binary>>   -> {account_id_key(AID), Key, true};
        _ -> {account_id_key(AID), File, false}
    end.

account_id_key(system) -> system;
account_id_key(root)   -> system;
account_id_key(ID) ->
    exodm_db:account_id_key(ID).

compute_checksum(Term) ->
    erlang:md5(term_to_binary(Term)).


find(Ix, V) ->
    find(get_aid(), Ix, V).

find(AID, Ix, V) ->
    kvdb:index_keys(?DB, tab_name(AID), Ix, V).

open_file_hook(AID, File, Opts) ->
    case try_read_file(AID, File, Opts) of
        {ok, _, {_, _, Bin}} ->
            open_bin_hook(AID, File, [{data, Bin}|Opts]);
        {error,_} = Err ->
            Err
    end.

try_read_file(AID, File, Opts) ->
    FBin = to_binary(filename:basename(File)),
    case FBin of
        <<"user.", Rest/binary>> ->
            try_file(AID, Rest, Opts);
        <<"system.", Rest/binary>> ->
            try_file(system, Rest, Opts);
        _ ->
            case try_file(AID, FBin, Opts) of
                {error, not_found} = NotFound ->
                    if AID == system ->
                            NotFound;
                       true ->
                            try_file(system, FBin, Opts)
                    end;
                Other ->
                    Other
            end
    end.

try_file(AID, File, Opts) ->
    try try_file_(AID, File, Opts)
    catch
	error:_ ->
	    {error, einval}
    end.

try_file_(AID, File0, _Opts) ->
    {File, Rev} = internal_filename(File0),
    Tab = tab_name(AID),
    case Rev of
        <<>> ->
            case latest_file_version(Tab, File) of
                {ok, FullName} ->
                    Key = kvdb_conf:join_key(FullName, <<"src">>),
                    case kvdb_conf:read(Tab, Key) of
                        {ok, Obj} ->
                            {ok, Tab, Obj};
                        {error, _} = Err ->
                            Err
                    end;
                error ->
                    {error, not_found}
            end;
        _ ->
            Key = kvdb_conf:join_key(File, <<"src">>),
            case kvdb_conf:read(Tab, Key) of
                {ok, Obj} ->
                    {ok, Tab, Obj};
                {error, _} = Err1 ->
                    Err1
            end
    end.


latest_file_version(Tab, File) ->
    ?debug("latest_file_version(~p, ~p)~n", [Tab, File]),
    Base = filename:basename(File, <<".yang">>),
    Sz = byte_size(Base),
    Key = <<Base/binary, "@:">>,
    ?debug("file base name ~p, size ~p, key ~p~n", [Base, Sz, Key]),
    case kvdb_conf:prev(Tab, Key) of
        {ok, {FoundKey, _, _}} ->
            ?debug("found key ~p~n", [FoundKey]),
            case kvdb_conf:split_key(FoundKey) of
                [<<Base:Sz/binary, _/binary>> = FullName|_] ->
                    ?debug("Latest version: ~s~n", [FullName]),
                    {ok, FullName};
                _ ->
                    error
            end;
        done ->
            error
    end.


internal_filename(File) ->
    case re:run(File, <<"(^[^@]+)@([-0-9]*)\\.yang\$">>,
                [{capture, all_but_first, binary}]) of
        {match, [_Base, Date]} ->
            {File, Date};
        nomatch ->
            Base = filename:basename(File, ".yang"),
            {<<Base/binary, "@.yang">>, <<>>}
    end.

open_bin_hook(AID0, File, Opts) ->
    AID = if AID0 == system -> <<"system">>;
             true -> exodm_db:account_id_key(AID0)
          end,
    Bin = proplists:get_value(data, Opts, <<>>),
    case file:open(filename:join(AID, File), [read, write, ram, binary]) of
	{ok, Fd} ->
	    ok = file:write(Fd, to_binary(Bin)),
	    {ok,0} = file:position(Fd,bof),
	    {ok, Fd};
	{error, _} ->
	    {error, enoent}
    end.

to_string(X) ->
    binary_to_list(exodm_db:to_binary(X)).

to_binary(X) ->
    exodm_db:to_binary(X).

get_aid() ->
    case exodm_db_session:get_aid() of
        root -> system;
        Aid -> Aid
    end.
            
