%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     exodm specific util functions 
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db).

-export([user_id_key/1]).
-export([device_id_key/1]).
-export([group_id_key/1]).
-export([list_key/2]).

-export([kvdb_key_split/1, kvdb_key_join/1, kvdb_key_join/2]).
-export([nc_key_split/1, nc_key_join/1, nc_key_join/2]).
-export([nc_to_kvdb_key/1]).

-export([to_binary/1]).
-export([binary_opt/2, uint32_opt/2, uint64_opt/2]).
-export([first_child/1, next_child/1]).
-export([all_children/1]).
-export([read/1, write/2]).

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

user_id_key(ID) ->
    list_to_binary([$u|id_key(ID)]).

group_id_key(ID) ->
    list_to_binary([$g|id_key(ID)]).

device_id_key(ID) ->
    list_to_binary([$x|id_key(ID)]).

%% fixme add list keys with predicate access
list_key(Name, Pos) when is_integer(Pos), Pos >= 0 ->
    IX = list_to_binary(integer_to_list(Pos)),
    NM = to_binary(Name),
    <<NM/binary, "[", IX/binary, "]">>.
    

id_key(ID) when is_integer(ID), ID >= 0, ID =< 16#ffffffff ->
    tl(integer_to_list(16#100000000+ID,16)).

kvdb_key_split(Key) when is_binary(Key) ->
    binary:split(Key, <<"*">>, [global]).

kvdb_key_join(A,B) ->
    join_parts(A,B,<<"*">>).

kvdb_key_join(Parts) ->
    join_parts(Parts, <<"*">>).

nc_key_join(A,B) ->
    join_parts(A,B,<<"/">>).
nc_key_join(Parts) ->
    join_parts(Parts, <<"/">>).

nc_to_kvdb_key(Key) ->
    case nc_key_split(Key) of
	[<<>> | Parts] -> kvdb_key_join(Parts);  %% kvdb does not use leading *
	Parts -> kvdb_key_join(Parts)
    end.

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
kvdb_key_drop(K) ->
    kvdb_key_join(reverse(tl(reverse(kvdb_key_split(K))))).

first_child(K) ->
    case kvdb_conf:next(<<K/binary,"*+">>) of
	{ok,{K1,_As,_Data}} ->
	    N = byte_size(K),
	    case erlang:split_binary(K1, N) of
		{K, <<$*,K2/binary>>} ->
		    [C|_] = kvdb_key_split(K2),
		    {ok,<<K/binary,$*,C/binary>>};
		{_, _} ->
		    done
	    end;
	done ->
	    done
    end.

next_child(K) ->
    case kvdb_conf:next(<<K/binary,"+">>) of
	{ok,{K1,_As,_Data}} ->
	    K0 = kvdb_key_drop(K),
	    N = byte_size(K0),
	    if N >= byte_size(K1) ->
		    done;
	       true ->
		    case erlang:split_binary(K1, N) of
			{K0, <<$*,K2/binary>>} ->
			    [C|_] = kvdb_key_split(K2),
			    {ok,<<K0/binary,$*,C/binary>>};
			{_, _} ->
			    done
		    end
	    end;
	done ->
	    done
    end.
    
all_children(K) ->
    case first_child(K) of
	{ok,C} -> all_children(C, [C]);
	done -> []
    end.

all_children(K, Acc) ->
    case next_child(K) of
	{ok,C} -> all_children(C, [C|Acc]);
	done -> reverse(Acc)
    end.

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

%% FIXME: validation must be in each model!!!
binary_opt(Key, Options) ->
    case proplists:lookup(Key,Options) of
	none -> 
	    <<>>;
	{Key,Value} when Key =:= ck; Key =:= sk ->
	    if is_integer(Value) ->
		    <<Value:64>>;
	       is_binary(Value), byte_size(Value) =:= 8 ->
		    Value
	    end;
	{activity,Value} when is_integer(Value) ->
	    <<Value:32>>;
	{_Key,Value} ->
	    to_binary(Value)
    end.

uint32_opt(Key, Options) ->
    case proplists:lookup(Key,Options) of
	none -> <<0:32>>;
	{Key,Value} -> <<Value:32>>
    end.

uint64_opt(Key, Options) ->
    case proplists:lookup(Key,Options) of
	none -> 0;
	{Key,Value} -> <<Value:64>>
    end.


write(Key,Value) ->
    kvdb_conf:write({Key,[],Value}).

read(Key) ->
    kvdb_conf:read(Key).

