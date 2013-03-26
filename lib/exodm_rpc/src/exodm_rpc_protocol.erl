-module(exodm_rpc_protocol).

-export([module/1,
	 mode/1]).
-export([create_table/0,
	 register_protocol/3, register_protocol/4,
	 remove_application/1]).

-record(protocol, {name,
		   module,
		   mode,
		   application}).

create_table() ->
    ets:new(?MODULE, [public, named_table, {keypos, 2},
		      {read_concurrency, true}]).

register_protocol(Application, Module, Mode)
  when is_atom(Application),
       is_atom(Module),
       Mode == direct; Mode == queued ->
    Protocol = atom_to_binary(Application, latin1),  % hard-coded for now
    register_protocol(Protocol, Application, Module, Mode).

register_protocol(Protocol, Application, Module, Mode)
  when is_binary(Protocol),
       is_atom(Application),
       is_atom(Module),
       Mode == queued; Mode == direct ->
    case ets:insert_new(?MODULE, #protocol{name = Protocol,
					   module = Module,
					   mode = Mode,
					   application = Application}) of
	true ->
	    ok;
	false ->
	    {error, {protocol_exists, [Protocol]}}
    end.

remove_application(Application) ->
    ets:select_delete(?MODULE, [{#protocol{application = Application, _ = '_'},
				 [], [true]}]),
    ok.

module(<<"exodm_bert">>) -> exodm_rpc_bert;
module(<<"exodm">>) -> exodm_rpc_exodm;
module(<<"exosense">>) -> exodm_rpc_exodm;
module(<<"ga_ck3">>) -> exodm_ck3_dispatch;
module(<<"exoport_http">>) -> exodm_rpc_exoport_http;
module(<<"exoport_ios">>) -> exodm_rpc_exoport_ios;
module(P) ->
    case ets:lookup(?MODULE, P) of
	[#protocol{module = M}] ->
	    M;
	[] ->
	    undefined
    end.

mode(<<"exodm_bert">>) -> queued;
mode(<<"exodm">>) -> direct;
mode(<<"exosense">>) -> direct;
mode(<<"ga_ck3">>) -> queued;
mode(<<"exoport_http">>) -> queued;
mode(<<"exoport_ios">>) -> direct;
mode(P) ->
    case ets:lookup(?MODULE, P) of
	[#protocol{mode = M}] ->
	    M;
	[] ->
	    undefined
    end.
