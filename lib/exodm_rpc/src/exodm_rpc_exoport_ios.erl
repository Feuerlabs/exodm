-module(exodm_rpc_exoport_ios).

-export([json_rpc/2]).

-include("exodm.hrl").

json_rpc({call, _Mod, 'device-register', Args}, Env) ->
    DevID0 = get_opt('device-id', Args, Env),
    [DevType, OSVsn, AppID] =
	[get_opt(K, Args, Env) ||
	    K <- [device_type, os_version, vendorAppIdentifier]],
    foo.



get_opt(Key, Args, Env) ->
    get_opt(Key, Args, Env, undefined).

get_opt(Key, Args, Env, Default) ->
    case lists:keyfind(Key, 1, Args) of
	{_, Value, _} ->
	    Value;
	false ->
	    case lists:keyfind(Key, 1, Env) of
		{_, Value} ->
		    Value;
		false ->
		    Default
	    end
    end.

