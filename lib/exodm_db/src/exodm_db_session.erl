%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
-module(exodm_db_session).

-behaviour(gen_server).

-export([authenticate/2, logout/1, is_active/1]).

-export([start_link/0,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-define(TAB, ?MODULE).

-record(st, {pending = dict:new(),
	     procs = dict:new()}).
-record(session, {user, hash, sha, timer}).

-define(INACTIVITY_TIMER, 60000).  % should be configurable?

authenticate(User, Pwd) ->
    gen_server:call(?MODULE, {auth, to_binary(User), to_binary(Pwd)}, 10000).

logout(User) ->
    gen_server:call(?MODULE, {logout, to_binary(User)}).

is_active(User) ->
    ets:member(?TAB, to_binary(User)).

to_binary(X) when is_binary(X) ->
    X;
to_binary(L) when is_list(L) ->
    list_to_binary(L).



start_link() ->
    create_ets(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_ets() ->
    case ets:info(?TAB, name) of
	undefined ->
	    ets:new(?TAB, [named_table, {keypos,2}, public,
			   {read_concurrency, true}]);
	_ ->
	    true
    end.

init([]) ->
    {ok, #st{}}.

handle_call({auth, U, P}, From, St) ->
    case ets:lookup(?TAB, U) of
	[] ->
	    {noreply, pending(U,[{From,P}], St)};
	[#session{hash = Hash, sha = Sha} = Session] ->
	    case sha(Hash, P) of
		Sha ->
		    reset_timer(Session),
		    {reply, true, St};
		_ ->
		    {reply, false, St}
	    end
    end;
handle_call({logout, U}, _From, St) ->
    ets:delete(?TAB, U),
    {reply, ok, St}.

handle_cast({first_auth, Pid, User, Res}, #st{pending = Pend,
					      procs = Procs} = St) ->
    Waiting = dict:fetch(User, Pend),
    St1 = St#st{pending = dict:erase(User, Pend),
		procs = dict:erase(Pid, Procs)},
    case Res of
	{true, Hash, Sha} ->
	    Session = #session{user = User,
			       hash = Hash,
			       sha = Sha,
			       timer = start_timer(User)},
	    ets:insert(?TAB, Session),
	    {noreply, process_pending(true, Waiting, User, St1)};
	false ->
	    {noreply, process_pending(false, Waiting, User, St1)}
    end.

handle_info({timeout, _, User}, St) ->
    io:fwrite("Session timeout (~s)~n", [User]),
    ets:delete(?TAB, User),
    {noreply, St};
handle_info({'DOWN',_,process,Pid,normal}, #st{procs = Procs} = St) ->
    %% not handling non-normal exists just yet. FIXME
    {noreply, St#st{procs = dict:erase(Pid, Procs)}}.


terminate(_, _) ->
    ok.

code_change(_, St, _) ->
    {ok, St}.

process_pending(Reply, Waiting, User, St) ->
  [{From1, P1}|Rest] = Waiting,
    gen_server:reply(From1, Reply),
    Same = [X || {_, P} = X <- Rest, P == P1],
    [gen_server:reply(From, Reply) || {From,_} <- Same],
    Different = Rest -- Same,
    case Different of
	[] -> St;
	[_|_] ->
	    case Reply of
		true ->
		    [gen_server:reply(From, false) || {From,_} <- Different],
		    St;
		false ->
		    %% Different may contain clients with the right password,
		    %% but at this point, we don't know what the right password
		    %% is. Re-run first_auth
		    pending(User, Different, St)
	    end
    end.

pending(U, [{_, P}|_] = Clients, #st{pending = Pend, procs = Procs} = St) ->
    case dict:is_key(U, Pend) of
	true ->
	    St#st{pending = dict:append_list(U, Clients, Pend)};
	false ->
	    Me = self(),
	    {Pid,_} = spawn_monitor(fun() ->
					    first_auth(U, P, Me)
				    end),
	    St#st{pending = dict:store(U, Clients, Pend),
		  procs = dict:store(Pid, U, Procs)}
    end.

first_auth(U, P, Parent) ->
    Res = case exodm_db_user:lookup_attr(0, U, '__password') of
	      [] ->
		  false;
	      [{_, Hash}] ->
		  {ok, HashStr} = bcrypt:hashpw(P, Hash),
                  case list_to_binary(HashStr) of
                      Hash ->
                          {true, Hash, sha(Hash, P)};
                      _ ->
                          false
		  end
	  end,
    gen_server:cast(Parent, {first_auth, self(), U, Res}).

sha(Hash, Passwd) ->
    crypto:sha_mac(Hash, Passwd).

start_timer(User) ->
    erlang:start_timer(?INACTIVITY_TIMER, self(), User).

reset_timer(#session{user = User, timer = TRef}) ->
    case erlang:cancel_timer(TRef) of
	Remain when is_integer(Remain) ->
	    ok;
	false ->
	    %% make sure to flush the timeout msg, in case already sent
	    receive
		{timeout, TRef, _} -> ok
	    after 0 -> ok
	    end
    end,
    NewTRef = start_timer(User),
    ets:update_element(?TAB, User, {#session.timer, NewTRef}).
