%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
-module(exodm_db_session).

-behaviour(gen_server).

-export([authenticate/2, logout/0, logout/1,
         is_active/0, is_active/1, refresh/0]).
-export([get_user/0, get_role/0, get_aid/0, get_auth/0]).
-export([spawn_child/1, spawn_link_child/1, spawn_monitor_child/1]).

-export([set_auth_as_user/1,
         set_trusted_proc/0]).

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

-record(session, {user,
                  aid,
                  role,
                  access = [],
                  hash,
                  sha,
                  timer}).

-define(INACTIVITY_TIMER, 5*60000).  % should be configurable?

-include_lib("lager/include/log.hrl").

%% @spec authenticate(Username, Password) -> {true, AID} | false
authenticate(User, Pwd) ->
    UName = to_binary(User),
    check_auth_(
      UName, gen_server:call(?MODULE, {auth, UName, to_binary(Pwd)}, 10000)).

check_auth_(UName, {true, AID, Role} = Result) ->
    put_auth_({UName, AID, Role}),
    Result;
check_auth_(_, false) ->
    false.

refresh() ->
    case gen_server:call(?MODULE, {refresh, get_user()}) of
        true ->
            ok;
        false ->
            erlang:error(unauthorized)
    end.

logout() ->
    logout(get('$exodm_user')).

logout(User) ->
    gen_server:call(?MODULE, {logout, to_binary(User)}).

is_active() ->
    is_active(get_user()).

is_active(User) ->
    case get('$exodm_trusted_proc') of
        true -> true;
        _ ->
            ets:member(?TAB, to_binary(User))
    end.

set_trusted_proc() ->
    put('$exodm_trusted_proc', true),
    ok.

is_trusted_proc() ->
    get('$exodm_trusted_proc') == true.


set_auth_as_user(User) ->
    case ets:lookup(?TAB, User) of
        [] ->
            case check_auth_(User, gen_server:call(
                                     ?MODULE,
                                     {make_user_active, to_binary(User)})) of
                {true,_,_} = Res ->
                    set_trusted_proc(),
                    Res;
                false ->
                    false
            end;
        [#session{aid = AID, role = Role}] ->
            put_auth_({User, AID, Role}),
            put('$exodm_system_proc', true),
            {true, AID, Role}
    end.


to_binary(X) when is_binary(X) ->
    X;
to_binary(L) when is_list(L) ->
    list_to_binary(L).

spawn_child(F) -> proc_lib:spawn(auth_f(F)).
spawn_link_child(F) -> proc_lib:spawn_link(auth_f(F)).
spawn_monitor_child(F) -> proc_lib:spawn_monitor(auth_f(F)).

get_user() -> if_active_(get_auth_(), fun({X,_,_}) -> X end).
get_aid()  -> if_active_(get_auth_(), fun({_,X,_}) -> X end).
get_role() -> if_active_(get_auth_(), fun({_,_,X}) -> X end).
get_auth() -> if_active_(get_auth_(), fun(X) -> X end).

if_active_({UName,_,_} = X, Ret) ->
    case is_active(UName) of
        true ->
            Ret(X);
        false ->
            erlang:error(unauthorized)
    end;
if_active_(_, _) ->
    case is_trusted_proc() of
        true -> superuser;
        false ->
            erlang:error(unauthorized)
    end.


get_auth_() -> get('$exodm_auth').
put_auth_({U, AID, Role}) -> put('$exodm_auth', {U, AID, Role}).

auth_f(F) ->
    Auth = get_auth(),  % checks if active
    fun() ->
            put_auth_(Auth),
            F()
    end.


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
        [#session{sha = undefined}] ->
            %% system processes have accessed user, but not authenticated
            %% sessions.
            {noreply, pending(U, [{From,P}], St)};
	[#session{hash = Hash, aid = AID, role = Role, sha = Sha} = Session] ->
	    case sha(Hash, P) of
		Sha ->
		    reset_timer(Session),
		    {reply, {true, AID, Role}, St};
		_ ->
		    {reply, false, St}
	    end
    end;
handle_call({make_user_active, U}, _, St) ->
    case ets:lookup(?TAB, U) of
        [] ->
            case first_auth_(U, no_password) of
                false ->
                    {reply, false, St};
                {true, Hash, undefined} ->
                    #session{aid = AID, role = Role} =
                        create_session(U, Hash, undefined),
                    {reply, {true, AID, Role}, St}
            end;
        [#session{aid = AID, role = Role}] = Session ->
            reset_timer(Session),
            {reply, {true, AID, Role}, St}
    end;
handle_call({refresh, U}, _From, St) ->
    case ets:lookup(?TAB, U) of
        [] ->
            {reply, false, St};
        [#session{} = Session] ->
            reset_timer(Session),
            {reply, true, St}
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
            #session{aid = AID, role = Role} =
                create_session(User, Hash, Sha),
	    {noreply, process_pending(
                        {true, AID, Role}, Waiting, User, St1)};
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
    Res = first_auth_(U, P),
    gen_server:cast(Parent, {first_auth, self(), U, Res}).

first_auth_(U, P) ->
    case exodm_db_user:lookup_attr(U, password) of
        [] ->
            false;
        [{_, Hash}] ->
            case P of
                no_password ->
                    %% means we're faking authentication of a system process
                    %% we can't make a sha hash, since we don't know the pwd.
                    {true, Hash, undefined};
                _ when is_binary(P) ->
                    {ok, HashStr} = bcrypt:hashpw(P, Hash),
                    case list_to_binary(HashStr) of
                        Hash ->
                            ?debug("bcrypt hash matches for ~p~n", [U]),
                            {true, Hash, sha(Hash, P)};
                        _ ->
                            ?debug("bcrypt hash doesn't match for ~p~n"
                                   "~p | ~p~n", [U, HashStr, Hash]),
                            false
                    end
            end
    end.

create_session(User, Hash, Sha) ->
    Sn = get_session_data(#session{user = User,
                                   hash = Hash,
                                   sha = Sha,
                                   timer = start_timer(User)}),
    ets:insert(?TAB, Sn),
    Sn.

get_session_data(#session{user = User} = S) ->
    %% [{_, AID}] = exodm_db_user:lookup_attr(User,<<"__aid">>),
    [{_,{AID,Role}}|_] = Access = exodm_db_user:list_access(User),
    S#session{aid = AID,
              role = Role,
              access = [{A,R} || {_,{A,R}} <- Access]}.

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
