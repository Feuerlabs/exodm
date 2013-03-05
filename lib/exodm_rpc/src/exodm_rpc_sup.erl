
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
    ] ++ push_servers().


push_servers() ->
    [push_server_spec(S) ||
	S <- lists:flatten(
		[S1 || {_, S1} <- setup:find_env_vars(push_servers)])].

push_server_spec({Name, {M,_,_} = MFA}) ->
    {Name, MFA, permanent, 5000, worker, [M]}.

