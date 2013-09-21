-module(exodm_rpc_dispatcher).
-behaviour(gen_server).

-export([check_queue/2,
	 asynch_check_queue/2]).
-export([attempt_dispatch/3,
	 attempt_dispatch/4]).
-export([start_link/3]).
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(st, {tab, mod,
	     queues = dict:new(),
	     pids = dict:new(),
	     pending = dict:new(),
	     jobs_queue = default}).

-include_lib("lager/include/log.hrl").

-spec check_queue(kvdb:tab_name(),
		  kvdb:queue_name()) -> pid() | {pending, pid()}.
check_queue(Tab, Q) ->
    call(Tab, {check_queue, Q}).

asynch_check_queue(Tab, Q) ->
    cast(Tab, {check_queue, Q}).

attempt_dispatch(Db, Tab, Q) ->
    attempt_dispatch(Db, Tab, Q, false).

attempt_dispatch(Db, Tab, Q, DoReply) when is_boolean(DoReply) ->
    call(Tab, {attempt_dispatch, Q, Db, DoReply}).

call(Tab0, Req) ->
    Tab = kvdb_lib:table_name(Tab0),
    case gproc:where({n,l, {?MODULE, Tab}}) of
	undefined ->
	    error(noproc);
	Pid when is_pid(Pid) ->
	    gen_server:call(Pid, Req)
    end.

cast(Tab0, Msg) ->
    Tab = kvdb_lib:table_name(Tab0),
    case gproc:where({n,l, {?MODULE, Tab}}) of
	undefined ->
	    error;
	Pid when is_pid(Pid) ->
	    gen_server:cast(Pid, Msg)
    end.

start_link(Tab0, M, JobQ) ->
    Tab = kvdb_lib:table_name(Tab0),
    gen_server:start_link(?MODULE, {Tab, M, JobQ}, []).

init({Tab0, M, JobQ} = Arg) ->
    Tab = kvdb_lib:table_name(Tab0),
    gproc:reg({n, l, {?MODULE, Tab}}),
    try
	ok = kvdb_conf:add_table(Tab, [{type, fifo},
				       {encoding, {raw,sext,sext}}]),
	kvdb_schema_events:notify_all_queues(kvdb:db(kvdb_conf), Tab),
	?debug("checking queues in ~p~n", [Tab]),
	St = check_queues(kvdb:first_queue(kvdb_conf, Tab),
			  #st{tab = Tab, mod = M,
			      jobs_queue = JobQ}),
	{ok, St}
    catch
	error:E ->
	    ?debug("*** ERROR: ~p:init(~p)~n"
		      "  E = ~p~n"
		      "  Trace = ~p~n",
		      [?MODULE, Arg, E, erlang:get_stacktrace()]),
	    error(E)
    end.

check_queues({ok, Q}, #st{tab = Tab} = St) ->
    ?debug("queue ~p in ~p not empty~n", [Q, Tab]),
    {_, St1} = spawn_dispatcher(Q, [], St),
    check_queues(kvdb:next_queue(kvdb_conf, Tab, Q), St1);
check_queues(done, St) ->
    St.


handle_info({gproc_ps_event,
	     {kvdb, kvdb_conf, Tab, queue_status}, {Q, not_empty}},
	    #st{tab = Tab} = St) ->
    ?debug("queue ~p in ~p not empty~n", [Q, Tab]),
    {_, St1} = check_queue_(Q, St),
    {noreply, St1};
handle_info({'DOWN', _, _, Pid, _}, #st{pids = Pids, queues = Qs,
					pending = Pending} = St) ->
    St1 = case dict:find(Pid, Pids) of
	      {ok, Q} ->
		  case dict:find(Q, Pending) of
		      {ok, ReplyTo} ->
			  {_, NewSt} =
			      spawn_dispatcher(
				Q, ReplyTo,
				St#st{pids = dict:erase(Pid, Pids),
				      queues = dict:erase(Q, Qs),
				      pending = dict:erase(Q, Pending)}),
			  NewSt;
		      error ->
			  St#st{pids = dict:erase(Pid, Pids),
				queues = dict:erase(Q, Qs)}
		  end;
	      _ ->
		  St
	  end,
    {noreply, St1};
handle_info(_Msg, St) ->
    ?debug("~p got ~p~n", [?MODULE, _Msg]),
    {noreply, St}.

handle_call({check_queue, Q}, _From, St) ->
    {Res, St1} = check_queue_(Q, St),
    {reply, Res, St1};
handle_call({attempt_dispatch, Q, Db, DoReply}, From, #st{queues = Qs} = St) ->
    case dict:find(Q, Qs) of
	error ->
	    {DbArg, Reply} = if DoReply -> {Db, [From]};
				true  -> {kvdb:db_name(Db), []}
			     end,
	    {Pid, St1} = spawn_dispatcher(Q, DbArg, Reply, St),
	    {reply, {ok, Pid}, St1};
	{ok, CurPid} ->
	    {_, St1} = mark_pending(CurPid, Q, From, St),
	    {reply, pending, St1}
    end;
handle_call(_, _, St) ->
    {noreply, St}.

handle_cast({check_queue, Q}, St) ->
    {_, St1} = check_queue_(Q, St),
    {noreply, St1};
handle_cast(_, St) -> {noreply, St}.

terminate(_, _) -> ok.
code_change(_, St, _) -> {ok, St}.


check_queue_(Q, #st{queues = Qs} = St) ->
    case dict:find(Q, Qs) of
	error ->
	    spawn_dispatcher(Q, [], St);
	{ok, CurPid} ->
	    mark_pending(CurPid, Q, false, St)
    end.

mark_pending(CurPid, Q, Reply, #st{pending = Pending} = St) ->
    P1 = case Reply of
	     false ->
		 case dict:find(Q, Pending) of
		     {ok, _} -> Pending;
		     error   -> dict:store(Q, [], Pending)
		 end;
	     From ->
		 dict:append(Q, From, Pending)
	 end,
    {{pending, CurPid}, St#st{pending = P1}}.

spawn_dispatcher(Q, Reply, St) ->
    spawn_dispatcher(Q, kvdb_conf, Reply, St).

spawn_dispatcher(Q, Db, Reply, #st{tab = Tab, pids = Pids, queues = Qs,
					 jobs_queue = JobsQ} = St) ->
    {Pid, _} = spawn_monitor(
		 fun() ->
			 run_dispatcher(JobsQ, Db, Tab, Q, Reply)
		 end),
    {Pid, St#st{pids = dict:store(Pid, Q, Pids),
		queues = dict:store(Q, Pid, Qs)}}.

run_dispatcher(JobsQ, Db, Tab, Q, Reply) ->
    try
	timer:sleep(500),
	dispatch(Db, Tab, Q, JobsQ, Reply)
    catch
	Type:Exception ->
	    ?error("Dispatch thread exception: ~p:~p~n~p~n",
		   [Type, Exception, erlang:get_stacktrace()])
    end.



dispatch(Db, <<"from_device">> = Tab, Q, JobsQ, Reply) ->
    exodm_db_session:set_auth_as_device(Q),
    %% Set Sessions to []; we don't care if the device is online for upstream
    pop_and_dispatch(Reply, Db, Tab, Q, JobsQ, []);
dispatch(Db, Tab, Q, JobsQ, Reply) ->
    ?debug("dispatch(~p, ~p)~n", [Tab, Q]),
    try
	case exodm_rpc_handler:device_sessions(Q) of
	    [_|_] = Sessions ->
		pop_and_dispatch(Reply, Db, Tab, Q, JobsQ, Sessions);
	    [] ->
                ?debug("no sessions for ~p", [Q]),
		done(Reply)
	end
    catch
	error:Reason ->
	    ?error("ERROR in ~p:dispatch(): ~p~n~p~n",
		   [?MODULE,Reason, erlang:get_stacktrace()])
    end.

pop_and_dispatch([], Db, Tab, Q, JobsQ, Sessions) ->
    kvdb:transaction(
      Db,
      fun(Db1) ->
	      until_done(Db1, Tab, Q, JobsQ, Sessions)
	   end);
pop_and_dispatch(Reply, Db, Tab, Q, JobsQ, Sessions) ->
    case kvdb:in_transaction(
	   Db,
	   fun(Db1) ->
		   pop_and_dispatch_(Reply, Db1, Tab, Q, JobsQ, Sessions)
	   end) of
	done ->
	    done;
	next ->
	    kvdb:transaction(
	      kvdb_conf,
	      fun(Db1) ->
		      until_done(Db1, Tab, Q, JobsQ, Sessions)
	      end)
    end.

until_done(Db, Tab, Q, JobsQ, Sessions) ->
    case pop_and_dispatch_([], Db, Tab, Q, JobsQ, Sessions) of
	done -> done;
	next -> until_done(Db, Tab, Q, JobsQ, Sessions)
    end.

done([]) ->
    done;
done(ReplyTo) ->
    [Pid ! {self(), ?MODULE, done} || {Pid,_} <- ReplyTo],
    done.


%% The first time, we may be popping the queue from within a transaction
%% store, and need to ack to the transaction master that we're done. Note that
%% *we* delete the object if successful, so the master may be committing an
%% empty set (which is ok).
pop_and_dispatch_(Reply, Db, Tab, Q, JobsQ, Sessions) ->
    ?debug("pop_and_dispatch_(~p, ~p, ~p, ~p, ~p)~n",
	   [Reply, Db, Tab, Q, Sessions]),
    jobs:run(
      JobsQ, fun() ->
		     pop_and_dispatch_run_(Reply, Db, Tab, Q, Sessions)
	     end).

pop_and_dispatch_run_(Reply, Db, Tab, Q, Sessions) ->
    %% case kvdb:pop(Db, Tab, Q) of
    case kvdb:peek(Db, Tab, Q) of
	done ->
	    done(Reply);
	{ok, QK, {_, Env, Req} = Entry} ->
	    ?debug("PEEK: Entry = ~p~n", [Entry]),
	    if Tab == <<"to_device">> ->
		    set_user(Env, Db);
	       true -> ok  % Already authenticated as device.
	    end,
	    {AID, DID} = exodm_db_device:dec_ext_key(Q),
	    %% FIXME: Since Q remains the same, we should only do this once.
	    Protocol = exodm_db_device:protocol(AID, DID),
	    do_dispatch(Reply, Db, Tab, Q, Sessions, QK, Env, Req, AID, DID, Protocol);
	Other ->
	    ?error("Unexpected result of peek(~p, ~p, ~p): ~p~n"
		   "aborting dispatch~n",
		   [Db, Tab, Q, Other]),
	    done(Reply)
    end.

do_dispatch(Reply, Db, <<"from_device">> = Tab, _Q, _Sessions,
	    QK, Env, Req, AID, DID, Protocol) ->
    Mod = exodm_rpc_protocol:module(Protocol),
    do_dispatch_(Mod, Reply, Db, Tab, Req, Env, AID, DID, undefined, QK);
do_dispatch(Reply, Db, <<"to_device">> = Tab, Q, Sessions,
	    QK, Env, Req, AID, DID, Protocol) ->
    case exodm_rpc_handler:find_device_session(Q, Protocol) of
	{ok, Pid} ->
	    Mod = exodm_rpc_protocol:module(Protocol),
	    ?debug("Calling ~p:dispatch(~p, ~p, ~p, ~p)~n",
		   [Mod, Env, AID, DID, Pid]),
	    do_dispatch_(Mod, Reply, Db, Tab, Req, Env, AID, DID, Pid, QK);
	error ->
	    ?error("No matching protocol session for ~p (~p)~n"
		   "Entry now blocking queue~n",
		   [{Env, Req}, Sessions]),
	    done(Reply)
    end.

do_dispatch_(Mod, Reply, Db, Tab, Req, Env, AID, DID, Pid, QK) ->
    case Mod:dispatch(Tab, Req, Env, AID, exodm_db:decode_id(DID), Pid) of
	error ->
	    done(Reply);
	_Result ->
	    ?debug("Valid result (~p); deleting queue object~n",
		   [_Result]),
	    _DeleteRes = kvdb:queue_delete(Db, Tab, QK),
	    ?debug("Delete (~p) -> ~p~n", [QK, _DeleteRes]),
	    %% done(Reply),
	    next
    end.

set_user(Env, Db) ->
    {user, UID} = lists:keyfind(user, 1, Env),
    {aid, AID} = lists:keyfind(aid, 1, Env),
    exodm_db_session:identify_trusted_process(AID, UID, Db).
    %% exodm_db_session:logout(),
    %% exodm_db_session:set_auth_as_user(AID, UID, Db).
