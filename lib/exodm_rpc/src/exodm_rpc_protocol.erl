-module(exodm_rpc_protocol).

-export([module/1,
	 mode/1]).


module(<<"exodm_bert">>) -> exodm_rpc_bert;
module(<<"exodm">>) -> exodm_rpc_exodm;
module(_) ->
    undefined.

mode(<<"exodm_bert">>) -> queued;
mode(<<"exodm">>) -> direct;
mode(_) ->
    undefined.
