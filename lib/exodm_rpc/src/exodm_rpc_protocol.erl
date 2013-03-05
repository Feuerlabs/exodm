-module(exodm_rpc_protocol).

-export([module/1,
	 mode/1]).


module(<<"exodm_bert">>) -> exodm_rpc_bert;
module(<<"exodm">>) -> exodm_rpc_exodm;
module(<<"exosense">>) -> exodm_rpc_exodm;
module(<<"ga_ck3">>) -> exodm_ck3_dispatch;
module(<<"exoport_http">>) -> exodm_rpc_exoport_http;
module(<<"exoport_ios">>) -> exodm_rpc_exoport_ios;
module(_) ->
    undefined.

mode(<<"exodm_bert">>) -> queued;
mode(<<"exodm">>) -> direct;
mode(<<"exosense">>) -> direct;
mode(<<"ga_ck3">>) -> queued;
mode(<<"exoport_http">>) -> queued;
mode(<<"exoport_ios">>) -> direct;
mode(_) ->
    undefined.
