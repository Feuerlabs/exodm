-module(exodm_rpc_protocol).

-export([module/1,
	 mode/1]).


module(<<"exodm_bert">>) -> exodm_rpc_bert;
module(<<"exodm">>) -> exodm_rpc_exodm;
module(<<"exosense">>) -> exodm_rpc_exodm;
module(<<"ga_ck3">>) -> exodm_ck3_dispatch;
module(_) ->
    undefined.

mode(<<"exodm_bert">>) -> queued;
mode(<<"exodm">>) -> direct;
mode(<<"exosense">>) -> direct;
mode(<<"ga_ck3">>) -> queued;
mode(_) ->
    undefined.
