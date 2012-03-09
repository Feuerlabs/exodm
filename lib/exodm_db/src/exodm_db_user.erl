%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense user manipulation
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_user).

-export([new/3, update/3, read/2, exist/2]).

-import(exodm_db, [write/2, binary_opt/2]).

%%
%% u<UID>/name     = ShortName
%% u<UID>/fullname = Name|Company
%% u<UID>/phone    = Phone number to contact
%% u<UID>/email    = Email to contact
%% u<UID>/skype    = Skype ID of contact
%%
new(UID, UName, Options) ->
    [_] = exodm_db:nc_key_split(UName),  %% validation!
    U = <<"user*", UName/binary,"*">>,
    write(<<U/binary,"name">>, UName),
    write(<<U/binary,"fullname">>,  binary_opt(fullname,Options)),
    write(<<U/binary,"phone">>,     binary_opt(phone,Options)),
    write(<<U/binary,"email">>,     binary_opt(email,Options)),
    write(<<U/binary,"skype">>,     binary_opt(skype,Options)),
    write(<<U/binary,"__password">>,binary_opt(password,Options)),
    %% ___uid MUST NEVER be accessed by user.
    DefUid = {uid,exodm_db:user_id_key(UID)},
    write(<<U/binary,"___uid">>,    binary_opt(uid,Options++[DefUid])),
    write(<<U/binary,"___gid">>,    binary_opt(gid,Options)),
    write(<<U/binary,"___access">>, binary_opt(access,Options)),
    ok.

update(UID, UName, Options) ->
    ok.

read(UID, UName) ->
    ok.

exist(UID, UName) ->
    ok.
