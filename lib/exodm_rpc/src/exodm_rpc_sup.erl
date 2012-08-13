
-module(exodm_rpc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(BERT_PORT, 9900).
-define(BERT_OPTS, [{ssl, true}]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Ch = base_children(),
    io:fwrite("EXODM_RPC_SUP: base_children() -> ~p~n", [Ch]),
    {ok, { {one_for_one, 5, 10}, base_children() }};
init({_Spec, _FromDevice, _ToDevice}) ->
    {ok, { {one_for_one, 5, 10}, base_children() }}.
	   %% base_children()
	   %% ++ [
	   %%     {exodm_rpc_spec, {exodm_rpc_spec, start_link, [Spec]},
	   %% 	permanent, 5000, worker, [exodm_rpc_spec]},
	   %%     {ck3_to_device, {exodm_rpc_dispatcher, start_link,
	   %% 		    [onf, a2b(ToDevice), ToDevice,
	   %% 		     exodm_rpc_to_device]},
	   %% 	permanent, 5000, worker, [exodm_rpc_dispatcher]},
	   %%     {ck3_from_device, {exodm_ck3_dispatcher, start_link,
	   %% 			  [kvdb_conf, a2b(FromDevice), FromDevice,
	   %% 		       exodm_rpc_from_device]},
	   %% 	permanent, 5000, worker, [exodm_ck3_dispatcher]}
	   %%    ]}}.

base_children() ->
    BertOpts = case application:get_env(exodm_rpc, bert_server) of
		   {ok, Env} ->
		       io:fwrite("bert_server - ~n"
				 "Env = ~p~n", [Env]),
		       Env;
		   _ ->
		       [{port, ?BERT_PORT} | ?BERT_OPTS]
	       end,
    [{bert_server, {bert_rpc_exec, start_link, [BertOpts]},
      permanent, 5000, worker, [bert_rpc_exec]},
     {to_device, {exodm_rpc_dispatcher, start_link,
		  [_Tab1 = to_device,
		   _M1 = exodm_rpc_to_device,
		   _JobQ1 = exodm_rpc_to_device]},
		permanent, 5000, worker, [exodm_rpc_dispatcher]},
     {from_device, {exodm_rpc_dispatcher, start_link,
		    [from_device,
		     exodm_rpc_from_device,
		     exodm_rpc_from_device]},
      permanent, 5000, worker, [exodm_rpc_dispatcher]}
    ].

a2b(A) when is_atom(A) ->
    atom_to_binary(A, latin1).
