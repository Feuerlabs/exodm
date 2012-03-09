%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     General config model
%%% @end
%%% Created :  9 Mar 2012 by Tony Rogvall <tony@rogvall.se>

-module(exodm_db_config).

-export([new/4, update/4, read/3, exist/3]).
%%    
%% /u<UID>/devices/x<DID>/config/<target>/<tree>
%%

%% Move to CK3!
new(UID, DID, Target, Options) ->
    Key = key(UID, DID, Target),
    insert(Key, <<"kill-switch">>, <<"0">>),
    insert(Key, <<"cksrv-address">>,<<"127.0.0.1">>),
    insert(Key, <<"cksrv-port">>, <<"4711">>),
    insert(Key, <<"wakeup-prof[1]*data">>,<<"01010102">>),
    insert(Key, <<"wakeup-prof[2]*data">>,<<>>),
    insert(Key, <<"wakeup-prof[3]*data">>,<<>>),
    insert(Key, <<"wakeup-prof[4]*data">>,<<>>),
    insert(Key, <<"wakeup-prof[5]*data">>,<<>>),
    insert(Key, <<"wakeup-prof[6]*data">>,<<>>),
    insert(Key, <<"wakeup-prof[7]*data">>,<<>>),
    insert(Key, <<"wakeup-prof[8]*data">>,<<>>),
    insert(Key, <<"wakeup-prof[9]*data">>,<<>>),
    insert(Key, <<"wakeup-prof[10]*data">>,<<>>),
    insert(Key, <<"wakeup-prof[11]*data">>,<<>>),
    insert(Key, <<"wakeup-prof[12]*data">>,<<>>),
    insert(Key, <<"wakeup-prof[13]*data">>,<<>>),
    insert(Key, <<"wakeup-prof[14]*data">>,<<>>),
    insert(Key, <<"wakeup-prof[15]*data">>,<<>>),
    insert(Key, <<"wakeup-prof[16]*data">>,<<>>),
    insert(Key, <<"door-lock-seq">>,<<"s:1:h150l150">>),
    insert(Key, <<"door-unlock-seq">>,<<"s:1:h300l300">>),
    insert(Key, <<"parking-off-intvl">>,<<"900">>),
    insert(Key, <<"gprs-login">>,<<"apn:username:password">>),
    insert(Key, <<"gprs-idle-disconnect">>,<<"900">>),
    insert(Key, <<"wifi-ssid">>,<<"Getaround">>),
    insert(Key, <<"wifi-ip">>,<<"192.168.0.1">>),
    insert(Key, <<"abuse_threshold">>,<<"ab1:x:>:10:50;ab2:y:>:10:50;ab3:z:>:10:50">>),
    insert(Key, <<"wpt-log-file">>,<<"/tmp/waypoint.ckgps">>),
    insert(Key, <<"wpt-log-size">>,<<"2097152">>),
    insert(Key, <<"wpt-log-intvl">>,<<"0.000000005">>),
    insert(Key, <<"abs-ts-log-intv">>,<<"100">>),
    insert(Key, <<"kill-switch-on">>,<<"s:2:h150l150">>),
    insert(Key, <<"kill-switch-off">>,<<"s:2:h300l300">>),
    insert(Key, <<"door-lock-setup">>,<<"c:1:o:l">>),
    insert(Key, <<"kill-switch-setup">>,<<"c:2:o:l">>),
    insert(Key, <<"boot-ui">>,<<"bsq:1:128:500">>),
    insert(Key, <<"active-ui">>,<<"lsq:1:1:t300">>),
    insert(Key, <<"gprs-connecting-ui">>,<<"lsq:2:2:t1000f1000">>),
    insert(Key, <<"gprs-connected-ui">>,<<"lsq:2:2:f100t100">>),
    insert(Key, <<"door-lock-ui">>,<<"lsq:3:10:1:f200t200">>),
    insert(Key, <<"door-unlock-ui">>,<<"lsq:3:10:4:f200t200">>),
    insert(Key, <<"gprs-connect-fail-ui">>,<<"lsq:2:5:t100f100">>),
    insert(Key, <<"max-proto-packet-size">>,<<"1300">>),
    insert(Key, <<"abs-wpt-log-intv">>,<<"10">>),
    insert(Key, <<"wpt-log-rpt-intv">>,<<"0">>),
    insert(Key, <<"request-timeout">>,<<"5">>),
    insert(Key, <<"request-resend-attempts">>,<<"3">>),
    insert(Key, <<"wpt-logging-active">>,<<"0">>),
    ok.

update(UID, DID, Target, Options) ->
    ok.

read(UID, DID, Target) ->
    ok.

exist(UID, DID, Target) ->
    exist(key(UID,DID,Target)).

exist(Key) ->
    case exodm_db:read(Key) of
	{ok, _} -> true;
	{error, not_found} -> false
    end.    

key(UID, DID, Target) ->
    U = exodm_db:user_id_key(UID),
    D = exodm_db:device_id_key(DID),
    T = exodm_db:to_binary(Target),
    exodm_db:kvdb_key_join([U, <<"devices">>, D, <<"config">>, T]).

insert(Key, Cfg, Value) ->
    exodm_db:write(exodm_db:kvdb_key_join([Key, Cfg]), Value).

