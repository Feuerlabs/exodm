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
new(AID, DID, Target, Options) ->
    exodm_db:in_transaction(
      fun(_) ->
	      new_(AID, DID, Target, Options)
      end).

new_(A, DID, Target, Options) ->
    Key = key(DID, Target),
    insert(A, Key, <<"kill-switch">>,
	   do_option(<<"kill-switch">>,Options,<<"0">>)),
    insert(A, Key,<<"cksrv-address">>,
	   do_option(<<"cksrv-address">>,Options,<<"127.0.0.1">>)),
    insert(A, Key, <<"cksrv-port">>,
	   do_option(<<"cksrv-port">>,Options, <<"4711">>)),
    insert(A, Key, <<"wakeup-prof[1]*data">>,
	   do_option(<<"wakeup-prof[1]*data">>,Options,<<"01010102">>)),
    insert(A, Key, <<"wakeup-prof[2]*data">>,
	   do_option(<<"wakeup-prof[2]*data">>,Options)),
    insert(A, Key, <<"wakeup-prof[3]*data">>,
	   do_option(<<"wakeup-prof[3]*data">>,Options)),
    insert(A, Key,<<"wakeup-prof[4]*data">>,
	   do_option(<<"wakeup-prof[4]*data">>,Options)),
    insert(A, Key,<<"wakeup-prof[5]*data">>,
	   do_option(<<"wakeup-prof[5]*data">>,Options)),
    insert(A, Key,<<"wakeup-prof[6]*data">>,
	   do_option(<<"wakeup-prof[6]*data">>,Options)),
    insert(A, Key,<<"wakeup-prof[7]*data">>,
	   do_option(<<"wakeup-prof[7]*data">>,Options)),
    insert(A, Key,<<"wakeup-prof[8]*data">>,
	   do_option(<<"wakeup-prof[8]*data">>,Options)),
    insert(A, Key,<<"wakeup-prof[9]*data">>,
	   do_option(<<"wakeup-prof[9]*data">>,Options)),
    insert(A, Key,<<"wakeup-prof[10]*data">>,
	   do_option(<<"wakeup-prof[10]*data">>,Options)),
    insert(A, Key,<<"wakeup-prof[11]*data">>,
	   do_option(<<"wakeup-prof[11]*data">>,Options)),
    insert(A, Key,<<"wakeup-prof[12]*data">>,
	   do_option(<<"wakeup-prof[12]*data">>,Options)),
    insert(A, Key,<<"wakeup-prof[13]*data">>,
	   do_option(<<"wakeup-prof[13]*data">>,Options)),
    insert(A, Key,<<"wakeup-prof[14]*data">>,
	   do_option(<<"wakeup-prof[14]*data">>,Options)),
    insert(A, Key,<<"wakeup-prof[15]*data">>,
	   do_option(<<"wakeup-prof[15]*data">>,Options)),
    insert(A, Key,<<"wakeup-prof[16]*data">>,
	   do_option(<<"wakeup-prof[16]*data">>,Options)),
    insert(A, Key,<<"door-lock-seq">>,
	   do_option(<<"door-lock-seq">>,Options,<<"s:1:h150l150">>)),
    insert(A, Key,<<"door-unlock-seq">>,
	   do_option(<<"door-unlock-seq">>,Options,<<"s:1:h300l300">>)),
    insert(A, Key,<<"parking-off-intvl">>,
	   do_option(<<"parking-off-intvl">>,Options,<<"900">>)),
    insert(A, Key,<<"gprs-login">>,
	   do_option(<<"gprs-login">>,Options,<<"apn:username:password">>)),
    insert(A, Key,<<"gprs-idle-disconnect">>,
	   do_option(<<"gprs-idle-disconnect">>,Options,<<"900">>)),
    insert(A, Key,<<"wifi-ssid">>,
	   do_option(<<"wifi-ssid">>,Options,<<"Getaround">>)),
    insert(A, Key,<<"wifi-ip">>,
	   do_option(<<"wifi-ip">>,Options,<<"192.168.0.1">>)),
    insert(A, Key,<<"abuse_threshold">>,
	   do_option(<<"abuse_threshold">>,Options,<<"ab1:x:>:10:50;ab2:y:>:10:50;ab3:z:>:10:50">>)),
    insert(A, Key,<<"abuse_threshold">>,
	   do_option(<<"wpt-log-file">>,Options,<<"/tmp/waypoint.ckgps">>)),
    insert(A, Key,<<"wpt-log-size">>,
	   do_option(<<"wpt-log-size">>,Options,<<"2097152">>)),
    insert(A, Key,<<"wpt-log-intvl">>,
	   do_option(<<"wpt-log-intvl">>,Options,<<"0.000000005">>)),
    insert(A, Key,<<"abs-ts-log-intv">>,
	   do_option(<<"abs-ts-log-intv">>,Options,<<"100">>)),
    insert(A, Key,<<"kill-switch-on">>,
	   do_option(<<"kill-switch-on">>,Options,<<"s:2:h150l150">>)),
    insert(A, Key,<<"kill-switch-off">>,
	   do_option(<<"kill-switch-off">>,Options,<<"s:2:h300l300">>)),
    insert(A, Key,<<"door-lock-setup">>,
	   do_option(<<"door-lock-setup">>,Options,<<"c:1:o:l">>)),
    insert(A, Key,<<"kill-switch-setup">>,
	   do_option(<<"kill-switch-setup">>,Options,<<"c:2:o:l">>)),
    insert(A, Key,<<"boot-ui">>,
	   do_option(<<"boot-ui">>,Options,<<"bsq:1:128:500">>)),
    insert(A, Key,<<"active-ui">>,
	   do_option(<<"active-ui">>,Options,<<"lsq:1:1:t300">>)),
    insert(A, Key,<<"gprs-connecting-ui">>,
	   do_option(<<"gprs-connecting-ui">>,Options,<<"lsq:2:2:t1000f1000">>)),
    insert(A, Key,<<"gprs-connecting-ui">>,
	   do_option(<<"gprs-connected-ui">>,Options,<<"lsq:2:2:f100t100">>)),
    insert(A, Key,<<"door-lock-ui">>,
	   do_option(<<"door-lock-ui">>,Options,<<"lsq:3:10:1:f200t200">>)),
    insert(A, Key,<<"door-unlock-ui">>,
	   do_option(<<"door-unlock-ui">>,Options,<<"lsq:3:10:4:f200t200">>)),
    insert(A, Key,<<"gprs-connect-fail-ui">>,
	   do_option(<<"gprs-connect-fail-ui">>,Options,<<"lsq:2:5:t100f100">>)),
    insert(A, Key,<<"gprs-connect-fail-ui">>,
	   do_option(<<"max-proto-packet-size">>,Options,<<"1300">>)),
    insert(A, Key,<<"abs-wpt-log-intv">>,
	   do_option(<<"abs-wpt-log-intv">>,Options,<<"10">>)),
    insert(A, Key,<<"wpt-log-rpt-intv">>,
	   do_option(<<"wpt-log-rpt-intv">>,Options,<<"0">>)),
    insert(A, Key,<<"request-timeout">>,
	   do_option(<<"request-timeout">>,Options,<<"5">>)),
    insert(A, Key,<<"request-resend-attempts">>,
	   do_option(<<"request-resend-attempts">>,Options,<<"3">>)),
    insert(A, Key,<<"wpt-logging-active">>,
	   do_option(<<"wpt-logging-active">>,Options,<<"0">>)),
    ok.

%%
%%
%%
update(AID, DID, Target, Options) ->
    exodm_db:in_transaction(fun(_) -> update_(AID, DID, Target, Options) end).

update_(A, DID, Target, Options) ->
    Key0 = key(DID, Target),
    case validate_options(Options) of
	true ->
	    lists:foreach(
	      fun({Key,Value}) ->
		      insert(A, Key0, Key, Value)
	      end, Options);
	false ->
	    error
    end.

validate_options([{Key,_Value} | Values]) ->
    try ck3:encode_config_key(Key) of
	_ ->
	    %% FIXME: validate value!
	    validate_options(Values)
    catch
	error:_ ->
	    false
    end;
validate_options([]) ->
    true.

%% read configuration data
read(A, DID, Target) ->
    Key = key(DID,Target),
    read_(A, Key, <<"kill-switch">>) ++
    read_(A, Key, <<"cksrv-address">>)  ++
    read_(A, Key, <<"cksrv-port">>)  ++
    read_(A, Key, <<"wakeup-prof[1]*data">>) ++
    read_(A, Key, <<"wakeup-prof[2]*data">>) ++
    read_(A, Key, <<"wakeup-prof[3]*data">>) ++
    read_(A, Key, <<"wakeup-prof[4]*data">>) ++
    read_(A, Key, <<"wakeup-prof[5]*data">>) ++
    read_(A, Key, <<"wakeup-prof[6]*data">>) ++
    read_(A, Key, <<"wakeup-prof[7]*data">>) ++
    read_(A, Key, <<"wakeup-prof[8]*data">>) ++
    read_(A, Key, <<"wakeup-prof[9]*data">>) ++
    read_(A, Key, <<"wakeup-prof[10]*data">>) ++
    read_(A, Key, <<"wakeup-prof[11]*data">>) ++
    read_(A, Key, <<"wakeup-prof[12]*data">>) ++
    read_(A, Key, <<"wakeup-prof[13]*data">>) ++
    read_(A, Key, <<"wakeup-prof[14]*data">>) ++
    read_(A, Key, <<"wakeup-prof[15]*data">>) ++
    read_(A, Key, <<"wakeup-prof[16]*data">>) ++
    read_(A, Key, <<"door-lock-seq">>) ++
    read_(A, Key, <<"door-unlock-seq">>) ++
    read_(A, Key, <<"parking-off-intvl">>) ++
    read_(A, Key, <<"gprs-login">>) ++
    read_(A, Key, <<"gprs-idle-disconnect">>) ++
    read_(A, Key, <<"wifi-ssid">>) ++
    read_(A, Key, <<"wifi-ip">>) ++
    read_(A, Key, <<"abuse_threshold">>) ++
    read_(A, Key, <<"wpt-log-file">>) ++
    read_(A, Key, <<"wpt-log-size">>) ++
    read_(A, Key, <<"wpt-log-intvl">>) ++
    read_(A, Key, <<"abs-ts-log-intv">>) ++
    read_(A, Key, <<"kill-switch-on">>) ++
    read_(A, Key, <<"kill-switch-off">>) ++
    read_(A, Key, <<"door-lock-setup">>) ++
    read_(A, Key, <<"kill-switch-setup">>) ++
    read_(A, Key, <<"boot-ui">>) ++
    read_(A, Key, <<"active-ui">>) ++
    read_(A, Key, <<"gprs-connecting-ui">>) ++
    read_(A, Key, <<"gprs-connected-ui">>) ++
    read_(A, Key, <<"door-lock-ui">>) ++
    read_(A, Key, <<"door-unlock-ui">>) ++
    read_(A, Key, <<"gprs-connect-fail-ui">>) ++
    read_(A, Key, <<"max-proto-packet-size">>) ++
    read_(A, Key, <<"abs-wpt-log-intv">>) ++
    read_(A, Key, <<"wpt-log-rpt-intv">>) ++
    read_(A, Key, <<"request-timeout">>) ++
    read_(A, Key, <<"request-resend-attempts">>) ++
    read_(A, Key, <<"wpt-logging-active">>).


exist(A, DID, Target) ->
    exist(A, key(DID,Target)).

exist(A, Key) ->
    case read(A, Key, <<"cksrv-address">>) of
	[] -> false;
	[_] -> true
    end.

key(DID, Target) ->
    D = exodm_db:encode_id(DID),
    T = exodm_db:to_binary(Target),
    exodm_db:kvdb_key_join([D, <<"config">>, T]).

insert(AID, Key, Cfg, Value) ->
    exodm_db:write(exodm_db_device:table(AID),
		   exodm_db:kvdb_key_join([Key, Cfg]), Value).

read_(AID, Key,Item) when is_binary(Item) ->
    Tab = exodm_db_device:table(AID),
    Key1 = exodm_db:kvdb_key_join(Key,Item),
    case exodm_db:read(Tab, Key1) of
	{ok,{_,_,Value}} -> [{Item,Value}];
	{error,not_found} -> []
    end.

do_option(Key, Options) ->
    do_option(Key, Options, <<>>).

do_option(Key, Options, Default) ->
    case proplists:lookup(Key, Options) of
	none -> Default;
	{_,Value} -> Value
    end.
