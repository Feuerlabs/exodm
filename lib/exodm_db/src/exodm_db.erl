%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     exodm specific util functions 
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db).

-export([init/0]).

-export([transaction/1, in_transaction/1]).

-export([group_id_key/1, group_id_num/1, group_id_value/1]).
-export([role_id_key/1, role_id_num/1, role_id_value/1]).
-export([account_id_key/1, account_id_num/1, account_id_value/1]).
-export([list_key/2]).
-export([table/2]).

-export([split_key/1, join_key/1, join_key/2]).
-export([nc_key_split/1, nc_key_join/1, nc_key_join/2]).
-export([nc_to_kvdb_key/1]).

-export([to_binary/1]).
-export([list_options/2, binary_opt/2, binary_opt/3, uint32_opt/2, uint64_opt/2,
	 uint32_bin/1, uint64_bin/1]).
-export([first_child/1, first_child/2,
	 next_child/1, next_child/2,
	 last_child/1, last_child/2]).
-export([fold_children/3]).
-export([fold_list/3, fold_list/4]).   % ([Tab,] Fun, Acc, Key)
-export([fold_list2/4, fold_list2/5]). % ([Tab,] Fun, Acc, Key, ListItem)
-export([last_in_list/1, last_in_list/2]).
-export([append_to_list/2, append_to_list/3]).
-export([all_children/1, all_children/2]).
-export([insert_alias/4]).
-export([add_table/2,
	 ix_alias/1, ix_name/1, ix_aid/1]).

-export([valid_id_string/1,
	 escape_key/1,
	 encode_id/1,
	 decode_id/1,
	 id_key_to_integer/1,
	 to_hex/1]).

-export([read/1, read/2,
	 write/2, write/3,
	 update_counter/2,
	 update_counter/3,
	 select/1, select/2, select/3]).

-import(lists, [reverse/1]).
%%
%% fixme: bitmap version is actually not that fast as I tought
%% break even is plenty of tests.
%%
-define(bm(A,B), (((1 bsl (((B)-(A))+1))-1) bsl (A))).

-define(bit(A),  (1 bsl (A))).

-define(is_set(BM,A), ((((BM) bsr (A)) band 1) =:= 1)).

-define(bm_lower,   ?bm($a,$z)).
-define(bm_upper,   ?bm($A,$Z)).
-define(bm_digit,   ?bm($0,$9)).
-define(bm_xdigit,  (?bm($0,$9) bor ?bm($A,$F) bor ?bm($a,$f))).
-define(bm_alpha,   (?bm($A,$Z) bor ?bm($a,$z))).
-define(bm_alnum,   (?bm_alpha bor ?bm_digit)).
-define(bm_wsp,     (?bit($\s) bor ?bit($\t))).
-define(bm_space,   (?bm_wsp bor ?bit($\r) bor ?bit($\n))).

-define(bm_id1, (?bm_alpha bor ?bit($_))).
-define(bm_id2, (?bm_id1 bor ?bm_digit bor ?bit($.) bor ?bit($-))).
-define(bm_id3, (?bm_id1 bor ?bm_digit bor ?bit($.) bor ?bit($-) bor ?bit($:))).

-define(is_lower(X), ?is_set(?bm_lower,(X))).
-define(is_upper(X), ?is_set(?bm_upper,(X))).
-define(is_digit(X), ?is_set(?bm_digit,(X))).
-define(is_wsp(X),   ?is_set(?bm_wsp,(X))).

-define(is_id1(X), ?is_set(?bm_id1,(X))).
-define(is_id2(X), ?is_set(?bm_id2,(X))).
-define(is_id3(X), ?is_set(?bm_id3,(X))).


init() ->
    exodm_db_account:init(),
    exodm_db_user:init(),
    exodm_db_system:init(),
    exodm_db_yang:init().


transaction(F) when is_function(F, 1) ->
    kvdb_conf:transaction(F).

in_transaction(F) when is_function(F, 1) ->
    kvdb_conf:in_transaction(F).

%% user_id_key(<<$u,$$, _/binary>> = UID) -> UID;
%% user_id_key(ID) when is_binary(ID) ->
%%     case ID of
%% 	<<$$, _/binary>> ->
%% 	    error(invalid_uid);
%% 	_ ->
%% 	    <<$u,$$, ID/binary>>
%%     end.

group_id_key("g" ++ _ = S) ->
    group_id_key(list_to_binary(S));
group_id_key(S) when is_list(S) ->
    group_id_key(list_to_integer(S));
group_id_key(<<$g, _/binary>> = GID) -> GID;
group_id_key(ID) ->
    list_to_binary([$g|id_key(ID)]).

group_id_num(Id) when is_integer(Id) ->
    Id;
group_id_num(<<I:32>>) -> I;
group_id_num(L) when is_list(L) -> list_to_integer(L);
group_id_num(<<$g, I/binary>>) ->
    list_to_integer(binary_to_list(I)).

group_id_value(GID) ->
    <<(group_id_num(GID)):32>>.

role_id_key(<<$r, _/binary>> = GID) -> GID;
role_id_key(ID) ->
    list_to_binary([$r|id_key(ID)]).

role_id_num(Id) when is_integer(Id) ->
    Id;
role_id_num(<<I:32>>) -> I;
role_id_num(L) when is_list(L) -> list_to_integer(L);
role_id_num(<<$r, I/binary>>) ->
    list_to_integer(binary_to_list(I)).

role_id_value(RID) ->
    <<(role_id_num(RID)):32>>.

account_id_key(<<$a, _/binary>> = AID) -> AID;
account_id_key(ID) ->
    list_to_binary([$a|id_key(ID)]).

account_id_num(Id) when is_integer(Id) -> Id;
account_id_num(<<I:32>>) -> I;
account_id_num(L) when is_integer(L) -> list_to_integer(L);
account_id_num(<<$a, I/binary>>) ->
    list_to_integer(binary_to_list(I)).

account_id_value(AID) ->
    <<(account_id_num(AID)):32>>.

%% device_id_key(<<$x, $$, _/binary>> = DID) -> DID;
%% device_id_key(ID) when is_binary(ID) ->
%%     case ID of
%% 	<<$$, _/binary>> ->
%% 	    error(invalid_did);
%% 	_ ->
%% 	    <<$x, $$, ID/binary>>
%%     end.
%%     list_to_binary([$x|id_key(ID)]);
%% device_id_key(ID) ->
%%     list_to_binary([$x|id_key(ID)]).


%% fixme add list keys with predicate access
list_key(Name, Pos) when is_integer(Pos), Pos >= 0 ->
    kvdb_conf:list_key(to_binary(Name), Pos).
    %% %% IX = list_to_binary(integer_to_list(Pos, 19)),
    %% NM = to_binary(Name),
    %% IX = list_to_binary(id_key(Pos)),
    %% <<NM/binary, "[", IX/binary, "]">>.

id_key(ID) when is_integer(ID), ID >= 0, ID =< 16#ffffffff ->
    tl(integer_to_list(16#100000000+ID,16));
id_key(<<ID:32/integer>>) ->
    tl(integer_to_list(16#100000000+ID,16)).

id_key_to_integer(I) when is_binary(I) ->
    list_to_integer(binary_to_list(I), 16).

split_key(Key) when is_binary(Key) ->
    kvdb_conf:split_key(Key).

join_key(A, B) ->
    kvdb_conf:join_key(A, B).

join_key(Parts) ->
    kvdb_conf:join_key(Parts).

nc_key_join(A,B) ->
    join_parts(A,B,<<"/">>).
nc_key_join(Parts) ->
    join_parts(Parts, <<"/">>).

nc_to_kvdb_key(Key) ->
    case nc_key_split(Key) of
	[<<>> | Parts] -> join_key(Parts);  %% kvdb does not use leading *
	Parts -> join_key(Parts)
    end.

table(AID, Type) ->
    join_parts(account_id_key(AID), Type, <<"_">>).

%%
%% nc
%% Split an external instance key into components
%%   /a/b/c/d
%% 
%%   /a[1]/b[2]
%%   /a[id='abcd']
%%   /a[id="abcd"][foo:id='xyz']
%%   /a[.="abcd"]                leaf-list entry
%%
%%   using @ as escape character for non id characters in predicate selector
%%   @@  = @
%%   @XY = 16#XY
%%
%%
nc_key_split(<<$/,C,Tail/binary>>) when ?is_id1(C) ->
    nc_key_split_id(Tail, [C], [<<"">>]);
nc_key_split(<<C,Tail/binary>>)    when ?is_id1(C) ->
    nc_key_split_id(Tail, [C], []).

nc_key_split_part(<<$/,C,Tail/binary>>,Acc) when ?is_id1(C) ->
    nc_key_split_id(Tail,[C],Acc);
nc_key_split_part(<<$/>>,Acc) ->
    reverse(Acc);
nc_key_split_part(<<>>,Acc) ->
    reverse(Acc).

%% nodeid part id [ "["  pred-expr | pos "]" ]
nc_key_split_id(<<$[,Tail/binary>>,RPart,Acc) ->
    nc_key_split_predicate(skip_wsp(Tail),[$[|RPart],Acc);
nc_key_split_id(<<C,Tail/binary>>,RPart,Acc) when ?is_id3(C) -> %% FIXME
    nc_key_split_id(Tail, [C|RPart], Acc);
nc_key_split_id(Tail,RPart,Acc) ->
    nc_key_split_part(Tail, r_add_part(RPart,Acc)).

%% predicate part 
nc_key_split_predicate(<<$.,$=,Tail/binary>>, RPart, Acc) ->
    nc_key_split_predicate_value(Tail, add_char($=,add_char($.,RPart)), Acc);
nc_key_split_predicate(<<C,Tail/binary>>,RPart,Acc) when ?is_id1(C) ->
    nc_key_split_predicate_id(Tail, [C|RPart], Acc);
nc_key_split_predicate(<<C,Tail/binary>>,RPart,Acc) when ?is_digit(C) ->
    nc_key_split_predicate_pos(Tail, [C|RPart], Acc).

%% keyname
%% FIXME
nc_key_split_predicate_id(<<C,Tail/binary>>,RPart,Acc) when ?is_id3(C) -> 
    nc_key_split_predicate_id(Tail, [C|RPart], Acc);
nc_key_split_predicate_id(<<$=,Tail/binary>>,RPart,Acc) ->
    nc_key_split_predicate_value(skip_wsp(Tail), add_char($=,RPart), Acc).

%% position
nc_key_split_predicate_pos(<<C,Tail/binary>>, RPart, Acc) when ?is_digit(C) ->
    nc_key_split_predicate_pos(Tail, [C|RPart], Acc);
nc_key_split_predicate_pos(<<Tail/binary>>, RPart, Acc) ->
    case skip_wsp(Tail) of
	<<$],$[,Tail2/binary>> ->
	    nc_key_split_predicate(Tail2, [$[,$]|RPart], Acc);
	<<$],Tail2/binary>> ->
	    nc_key_split_part(Tail2, r_add_part([$]|RPart],Acc))
    end.

%% we may want to sort list keys to get uniq key?
nc_key_split_predicate_value(<<$",Tail/binary>>, RPart, Acc) ->
    nc_key_split_predicate_string(Tail, $", add_char($",RPart), Acc);
nc_key_split_predicate_value(<<$',Tail/binary>>, RPart, Acc) ->
    nc_key_split_predicate_string(Tail, $', add_char($',RPart), Acc).

nc_key_split_predicate_string(<<Q,Tail/binary>>,Q,RPart,Acc) ->
    case skip_wsp(Tail) of
	<<$],$[,Tail2/binary>> ->
	    nc_key_split_predicate(Tail2, [$[,$]|add_char(Q,RPart)], Acc);
	<<$],Tail2/binary>> ->
	    nc_key_split_part(Tail2, r_add_part([$]|add_char(Q,RPart)],Acc))
    end;
nc_key_split_predicate_string(<<$\\,$n,Tail/binary>>,$",RPart,As) ->
    nc_key_split_predicate_string(Tail,$",[$n|add_char($\\,RPart)], As);
nc_key_split_predicate_string(<<$\\,$t,Tail/binary>>,$",RPart,As) ->
    nc_key_split_predicate_string(Tail,$",[$t|add_char($\\,RPart)], As);
nc_key_split_predicate_string(<<$\\,$\\,Tail/binary>>,$",RPart,As) ->
    nc_key_split_predicate_string(Tail,$",add_char($\\,add_char($\\,RPart)),As);
nc_key_split_predicate_string(<<$\\,$",Tail/binary>>,$",RPart,As) ->
    nc_key_split_predicate_string(Tail,$",add_char($",add_char($\\,RPart)),As);
nc_key_split_predicate_string(<<$\\,C,Tail/binary>>,$",RPart,As) ->
    nc_key_split_predicate_string(Tail,$",add_char(C,add_char($\\,RPart)), As);
nc_key_split_predicate_string(<<C,Tail/binary>>, Q, RPart, As) ->
    nc_key_split_predicate_string(Tail,Q, add_char(C,RPart), As).


%% Remeber to hint bjorng about r_list_to_binary again?
r_add_part(RAcc, Parts) ->
    [list_to_binary(reverse(RAcc)) | Parts].

join_parts(A, B, Sep) when is_binary(A), is_binary(B), is_binary(Sep) ->
    <<A/binary,Sep/binary,B/binary>>.

join_parts([Part],_Sep) -> Part;
join_parts([Part|Ps], Sep) -> join_parts(Part, join_parts(Ps,Sep), Sep);
join_parts([], _) -> <<>>.

%% drop last key component
%% kvdb_key_drop(K) ->
%%     join_key(reverse(tl(reverse(split_key(K))))).

first_child(K) ->
    kvdb_conf:first_child(K).

first_child(Tab, K) ->
    kvdb_conf:first_child(Tab, K).

next_child(K) ->
    kvdb_conf:next_child(K).

next_child(Tab, K) ->
    kvdb_conf:next_child(Tab, K).

last_child(K) ->
    last_child(<<"data">>, K).

last_child(Tab, K) ->
    case kvdb_conf:prev(Tab, <<K/binary, "*~">>) of
	{ok, {K1,_As,_Data}} when byte_size(K1) > byte_size(K) ->
	    N = byte_size(K),
	    case erlang:split_binary(K1, N) of
		{K, <<$*,K2/binary>>} ->
		    [C|_] = split_key(K2),
		    {ok,<<K/binary,$*,C/binary>>};
		{_, _} ->
		    done
	    end;
	_ ->
	    done
    end.


all_children(K) ->
    all_children(<<"data">>, K).

all_children(Tab, K) ->
    reverse(fold_children(Tab, fun(Child,Acc) -> [Child|Acc] end, [], K)).

fold_children(Fun, Acc, K) ->
    fold_children(<<"data">>, Fun, Acc, K).

fold_children(Tab, Fun, Acc, K) ->
    fold_children_(Tab, Fun, Acc, first_child(Tab, K)).

fold_children_(Tab, Fun, Acc, {ok,K}) ->
    Acc1 = Fun(K, Acc),
    fold_children_(Tab, Fun, Acc1, next_child(Tab, K));
fold_children_(_Tab, _Fun, Acc, done) ->
    Acc.

%% Fold over list items, ListItem is given with base name
%% Fun :: fun(I::integer(), Key::binary(), Acc::any()) -> _NewAcc::any().
%%
fold_list2(Fun, Acc, Key, ListItem) when is_function(Fun, 3) ->
    fold_list2(<<"data">>, Fun, Acc, Key, ListItem).

fold_list2(Tab, Fun, Acc, Key, ListItem) when is_function(Fun, 3) ->
    kvdb_conf:fold_list(Tab, Fun, Acc, join_key(Key, to_binary(ListItem))).
    %% K = join_key(Key, to_binary(ListItem)),
    %% fold_list_(Tab, Fun, Acc, K, next_child(Tab, K)).

fold_list(Fun, Acc, Key) when is_function(Fun, 3) ->
    fold_list(<<"data">>, Fun, Acc, Key).

fold_list(Tab, Fun, Acc, Key) when is_function(Fun, 3) ->
    kvdb_conf:fold_list(Tab, Fun, Acc, Key).
%%     %% assume the Key is refereing to the list basename without []
%%     fold_list_(Tab, Fun, Acc, Key, next_child(Tab, Key)).

%% fold_list_(Tab, Fun, Acc, Key, {ok,Key1}) ->
%%     N = byte_size(Key),
%%     N1 = byte_size(Key1),
%%     N2 = (N1-N)-2,
%%     case Key1 of
%% 	<<Key:N/binary,"[",Pos:N2/binary,"]">> ->
%% 	    %% FIXME: this assumes simple lists with position,
%% 	    %% make this work for predicate items as well.
%% 	    %% I = list_to_integer(binary_to_list(Pos)),
%% 	    I = id_key_to_integer(Pos),
%% 	    Acc1 = Fun(I, Key1, Acc),
%% 	    fold_list_(Tab, Fun, Acc1, Key, next_child(Tab, Key1));
%% 	_ ->
%% 	    reverse(Acc)
%%     end;
%% fold_list_(_Tab, _Fun, Acc, _Key, done) ->
%%     reverse(Acc).

insert_alias(Tab, Base, I, Alias0) when is_integer(I), I >= 0 ->
    Alias = to_binary(Alias0),
    case read(Tab, join_key(Alias0, <<"name">>)) of
	{error, not_found} ->
	    case kvdb:index_keys(kvdb_conf, Tab, alias, Alias) of
		[] ->
		    K = join_key(
			  [Base, list_key(alias, I), <<"__alias">>]),
		    write(Tab, K, Alias);
		[_|_] ->
		    error({alias_exists, Alias})
	    end;
	{ok, _} ->
	    error({alias_exists, Alias})
    end.

%% Calculates the next unused position (following the highest pos), creates
%% a list key, and calls F(ListKey).
append_to_list(Base, F) when is_binary(Base), is_function(F, 1) ->
    append_to_list(<<"data">>, Base, F).

append_to_list(Tab, Base, F) when is_binary(Base), is_function(F, 1) ->
    {ok, Last} = kvdb_conf:last_list_pos(Tab, Base),
    Pos = Last + 1,
    Key = list_key(Base, Pos),
    F(Key).

%% append_to_list(Base, SubK, V) ->
%%     append_to_list(<<"data">>, Base, SubK, V).

%% append_to_list(Tab, Base, SubK, V) ->
%%     case last_in_list(Tab, Base) of
%% 	[] ->
%% 	    write(Tab, list_key(Base, 1), V),
%% 	    1;
%% 	[{K, _As, _}] ->
%% 	    LastPos = list_key_pos(Base, K),
%% 	    NewPos = LastPos+1,
%% 	    NewKey = join_key(list_key(Base, NewPos),
%% 			      to_binary(SubK)),
%% 	    write(Tab, NewKey, V),
%% 	    NewPos
%%     end.

-spec last_in_list(kvdb:key()) -> {ok, kvdb:obj()} | {error, any()}.
last_in_list(Base) ->
    last_in_list(<<"data">>, Base).

-spec last_in_list(kvdb:table(), kvdb:key()) ->
			  {ok, kvdb:obj()} | {error, any()}.
last_in_list(Tab, Base) ->
    {ok, Last} = kvdb_conf:last_list_pos(Tab, Base),
    read(Tab, list_key(Base, Last)).

    %% Sz = byte_size(Base),
    %% case kvdb_conf:prev(Tab, <<Base/binary, "[:">>) of
    %% 	{ok, {<<Base:Sz/binary, $[, _/binary>>, _, _} = Obj} ->
    %% 	    [Obj];
    %% 	_ ->
    %% 	    []
    %% end.

%% list_key_pos(Base, Key) ->
%%     SzB = byte_size(Base),
%%     <<Base:SzB/binary, Rest/binary>> = Key,
%%     [Ix|_] = split_key(Rest),
%%     SzI = byte_size(Ix),
%%     N = SzI - 2,
%%     <<"[", I:N/binary, "]">> = Ix,
%%     id_key_to_integer(I).


%% decr(infinity) ->
%%     infinity;
%% decr(I) when is_integer(I), I > 0 ->
%%     I-1.



to_binary(Value) when is_atom(Value) ->
    erlang:atom_to_binary(Value, latin1);
to_binary(Value) when is_list(Value) ->
    iolist_to_binary(Value);
to_binary(Value) when is_binary(Value) ->
    Value.

skip_wsp(<<C,Tail/binary>>) when ?is_wsp(C) ->
    skip_wsp(Tail);
skip_wsp(Tail) ->
    Tail.

%% Encoding users @ as an escape character followed by the escaped char
%% hex-coded (e.g. "@" -> "@40", "/" -> "@2F"). In order to know that the
%% id has been encoded - so we don't encode it twice - we prepend a '='
%% to the encoded id. Since '=' lies between ASCII numbers and letters
%% (just as '@' does), it won't upset the kvdb sort order.
%%
%% As a consequence, no unescaped ID string may begin with '='.
%%
valid_id_string(<<C, _/binary>>) ->
    not lists:member(C, ":;<>=?").

escape_key(Key) ->
    kvdb_conf:escape_key(Key).

encode_id(L) when is_list(L) ->
    encode_id(list_to_binary(L));
encode_id(I) when is_integer(I) ->
    encode_id(list_to_binary(integer_to_list(I)));
encode_id(Bin) when is_binary(Bin) ->
    kvdb_conf:escape_key(Bin).

decode_id(<<$=, Enc/binary>>) ->
    decode_id_(Enc);
decode_id(Bin) when is_binary(Bin) ->
    Bin.

decode_id_(<<$@, A, B, Rest/binary>>) ->
    <<(list_to_integer([A,B], 16)):8/integer, (decode_id_(Rest))/binary>>;
decode_id_(<<C, Rest/binary>>) ->
    <<C, (decode_id_(Rest))/binary>>;
decode_id_(<<>>) ->
    <<>>.



%% id_char(C) ->
%%     case ?is_id2(C) of
%% 	true -> <<C>>;
%% 	false when C =< 255 ->
%% 	    <<$@, (to_hex(C bsr 4)):8/integer, (to_hex(C)):8/integer >>
%%     end.

add_char($@,Part) -> [$@,$@|Part];
add_char(C,Part) ->
    case ?is_id2(C) of
	true -> 
	    [C|Part];
	false ->
	    [to_hex(C),to_hex(C bsr 4),$@|Part]
    end.

to_hex(C) ->    
    element((C band 16#f)+1, {$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,
			      $A,$B,$C,$D,$E,$F}).


list_options(Key, Options) ->
    Values =
	lists:flatmap(fun({K, V}) when K==Key, is_binary(V) -> [V];
			 ({K, [{I,_}|_] = Vals}) when
				K==Key, is_integer(I) -> Vals;
			 ({K, [V|_] = Vals}) when K==Key, is_binary(V) -> Vals;
			 (_) -> []
		      end, Options),
    {Numbered, Unnumbered} =
	lists:partition(fun(X) -> is_tuple(X) end, Values),
    merge_list_options(1, lists:keysort(1, Numbered), Unnumbered).

merge_list_options(P, [{P,V}|Ns], UNs) ->
    [{P,V}|merge_list_options(P+1, Ns, UNs)];
merge_list_options(P, [{P1,_}|_] = Ns, UNs) when P < P1 ->
    case UNs of
	[] -> merge_list_options(P1, Ns, []);
	[H|T] ->
	    [{P,H}|merge_list_options(P+1, Ns, T)]
    end;
merge_list_options(P, [], [H|UNs]) ->
    [{P,H}|merge_list_options(P+1, [], UNs)];
merge_list_options(_, [], []) ->
    [].

%% FIXME: validation must be in each model!!!
binary_opt(Key, Options) ->
    binary_opt(Key, Options, <<>>).

binary_opt(Key, Options, Default) ->
    to_binary(proplists:get_value(Key,Options,Default)).

uint32_opt(Key, Options) ->
    case proplists:lookup(Key,Options) of
	none -> <<0:32>>;
	{Key,Value} -> uint32_bin(Value)
    end.

uint64_opt(Key, Options) ->
    case proplists:lookup(Key,Options) of
	none -> 0;
	{Key,Value} -> uint64_bin(Value)
    end.

uint32_bin(I) when is_integer(I) ->
    <<I:32>>;
uint32_bin(L) when is_list(L) ->
    <<(list_to_integer(L)):32>>;
uint32_bin(<<_:32>> = Bin) ->
    Bin.

uint64_bin(I) when is_integer(I) ->
    <<I:64>>;
uint64_bin(L) when is_list(L) ->
    <<(list_to_integer(L)):64>>;
uint64_bin(<<_:64>> = Bin) ->
    Bin.



write(Key,Value) ->
    kvdb_conf:write({Key,[],Value}).

write(Tab, Key,Value) ->
    kvdb_conf:write(Tab, {Key,[],Value}).

read(Key) ->
    kvdb_conf:read(Key).

read(Tab, Key) ->
    kvdb_conf:read(Tab, Key).

update_counter(Key, Incr) ->
    update_counter(<<"data">>, Key, Incr).

update_counter(Tab, Key, Incr) ->
    kvdb_conf:update_counter(Tab, Key, Incr).

select(Pat) ->
    select(<<"data">>, Pat).

select(Tab, Pat) ->
    kvdb:select(kvdb_conf, Tab, Pat).

select(Tab, Pat, Limit) ->
    kvdb:select(kvdb_conf, Tab, Pat, Limit).

add_table(Name, Indexes) ->
    Opts = case [index_def(I) || I <- Indexes] of
	       [] ->
		   [];
	       [_|_] = Ixes ->
		   [{index, Ixes}]
	   end,
    kvdb_conf:add_table(Name, Opts).

index_def(alias) ->
    {alias, each, {exodm_db, ix_alias}};
index_def(name) ->
    {name, each, {exodm_db, ix_name}};
index_def(aid) ->
    {aid, each, {exodm_db, ix_aid}};
index_def(Other) ->
    Other.


ix_alias({K, _, V}) ->
    ix_list(K, V, <<"=__alias">>, <<"=alias">>).

ix_name({K, _, V}) ->
    ix_leaf(K,V,<<"=name">>).

ix_aid({K, _, V}) ->
    ix_leaf(K,V,<<"__aid">>).

ix_leaf(K, V, Match) ->
    case lists:reverse(kvdb_conf:raw_split_key(K)) of
	[Match, _] ->
	    %% NOTE: only "top-level" matches are indexed
	    io:fwrite("ix_leaf (~p): ~p~n", [K, V]),
	    [V];
	_ ->
	    []
    end.

ix_list(K, V, Key, Base) ->
    BSz = byte_size(Base),
    case lists:reverse(kvdb_conf:raw_split_key(K)) of
	[Key, <<Base:BSz/binary, "[",_/binary>>, _] ->
	    %% NOTE: only "top-level" aliases are indexed
	    io:fwrite("list ix (~p): ~p~n", [K, V]),
	    [V];
	_ ->
	    []
    end.
