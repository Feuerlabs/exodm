-module(exodm_rpc_to_device).

-export([dispatch/3]).

-include_lib("lager/include/log.hrl").

-define(dbg(F,A), ?debug("~p " ++ F, [self()|A])).

dispatch(Db, Tab, DID) ->
    %% DID = Q
    case exodm_rpc:is_device_active(DID) of
	{true, Pid} ->
	    case kvdb:prel_pop(Db, Tab, DID) of
		done ->
		    done;
		{ok, Entry, AbsKey} ->
		    ?debug("PREL_POP: Entry = ~p~n"
			   "  AbsKey = ~p~n", [Entry, AbsKey]),
		    case process_entry(Entry, Pid) of
			ok ->
			    kvdb:delete(Db, Tab, AbsKey),
			    dispatch(Db, Tab, DID);
			error ->
			    done
		    end
	    end;
	false ->
	    done
    end.

process_entry({_,_,{request,_Env,Call}} = _Entry, Pid) ->
    ?dbg("process_entry(~p)~n", [_Entry]),
    {call,Mod,Fun,Args} = Call,
    {_, DID} = lists:keyfind('device-id', 1, Args),
    case nice_bert_rpc:call(Pid, Mod, Fun, Args) of
	{reply, Reply, _} ->
	    io:fwrite("GOT REPLY: ~p~n", [Reply]),
	    exodm_rpc:queue_message(from_device, [{'device-id', DID}],
				    Reply),
	    ok;
	Other ->
	    ?error("Failed RPC attempt: ~p~n", [Other]),
	    error
    end.

