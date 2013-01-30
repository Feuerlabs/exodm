%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
-module(exodm_db_session).

-behaviour(gen_server).

-export([authenticate/2, logout/0, logout/1,
         is_active/0, is_active/1, refresh/0]).
-export([get_user/0, get_role/0, get_aid/0, get_auth/0]).
-export([remove_user_session/1]).
-export([spawn_child/1, spawn_link_child/1, spawn_monitor_child/1]).

-export([remove_account/1]).

-export([set_auth_as_user/1, set_auth_as_user/2, set_auth_as_user/3,
         set_auth_as_device/1,
         set_trusted_proc/0,unset_trusted_proc/0,
         is_trusted_proc/0]).

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
-define(dbg(F,A), ?debug("~p " ++ F, [self()|A])).

%% @spec authenticate(Username, Password) -> {true, AID} | false
authenticate(User, Pwd) ->
    UName = to_binary(User),
    check_auth_(
      UName, gen_server:call(?MODULE, {auth, UName, to_binary(Pwd)}, 10000)).

remove_account(AID) ->
    gen_server:call(?MODULE, {remove_account, AID}).

check_auth_(UName, {true, AID, Role} = Result) ->
    put_auth_({UName, AID, Role}),
    Result;
check_auth_(_, false) ->
    false.

%%--------------------------------------------------------------------
%% @doc
%% Removes any existing user session in the ets table..
%% Stops execution if unauthorized.
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_user_session(UID::binary()) -> ok | no_return().

remove_user_session(UID) ->
    case gen_server:call(?MODULE, {remove_user_session, get_auth(), UID}) of
        true ->
            ok;
        false ->
            erlang:error(unauthorized)
    end.

refresh() ->
    case gen_server:call(?MODULE, {refresh, get_user()}) of
        true ->
            ok;
        false ->
            erlang:error(unauthorized)
    end.

logout() ->
    case get_auth_() of
        {User, _, _} ->
            do_logout(User),
            del_auth_(),
            unset_trusted_proc(),
            ok;
        undefined ->
            ok
    end.

logout(User0) ->
    User = to_binary(User0),
    case get_auth_() of
        {User, _, _} ->
            do_logout(User),
            del_auth_(),
            unset_trusted_proc();
        _ ->
            case is_trusted_proc() of
                true ->
                    do_logout(User),
                    ok;
                false ->
                    error(unauthorized)
            end
    end,
    ok.

do_logout(User) ->
    gen_server:call(?MODULE, {logout, User}).


is_active() ->
    is_active(get_user()).

is_active(User) ->
    case get('$exodm_trusted_proc') of
        true -> true;
        _ ->
            K = case User of
                    {did,_} -> User;
                    _ -> to_binary(User)
                end,
            ets:member(?TAB, K)
    end.

set_trusted_proc() ->
    put('$exodm_trusted_proc', true),
    ok.

unset_trusted_proc() ->
    erase('$exodm_trusted_proc').

is_trusted_proc() ->
    get('$exodm_trusted_proc') == true.


set_auth_as_user(User) ->
    set_auth_as_user(User, kvdb_conf, false).

set_auth_as_user(User, Db) ->
    set_auth_as_user(User, Db, false).

set_auth_as_user(User, Db, Sticky) ->
    case ets:lookup(?TAB, User) of
        [] ->
            case check_auth_(
                   User, gen_server:call(
                           ?MODULE,
                           {make_user_active, to_binary(User), Db})) of
                {true,AID,Role} = Res ->
                    put_auth_({User, AID, Role}),
                    set_sticky_flag(Sticky),
                    Res;
                false ->
                    false
            end;
        [#session{aid = AID, role = Role}] ->
            put_auth_({User, AID, Role}),
            set_sticky_flag(Sticky),
            {true, AID, Role}
    end.

set_sticky_flag(false) ->
    ok;
set_sticky_flag(true) ->
    put('$exodm_sticky', true).

is_sticky() ->
    case get('$exodm_sticky') of
        true ->
            true;
        _ ->
            false
    end.


set_auth_as_device(DID0) ->
    ?dbg("set_auth_as_device(~p)~n", [DID0]),
    {AID, DID} = exodm_db_device:dec_ext_key(DID0),
    put_auth_({{did,DID}, AID, undefined}),
    {true, AID, undefined}.


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

if_active_({{did,_},_,_} = X, Ret) ->
    Ret(X);
if_active_({UName,_,_} = X, Ret) ->
    ?dbg("if_active_(~p, Ret, ~p)~n", [X, Ret]),
    case is_active(UName) of
        true ->
            Ret(X);
        false ->
            case is_trusted_proc() of
                true ->
                    Ret(X);
                false ->
                    ?dbg("NOT active (~p)~n", [UName]),
                    erlang:error(unauthorized)
            end
    end;
if_active_(_, _) ->
    case is_trusted_proc() of
        true -> root;  %% still necessary ?
        false ->
            erlang:error(unauthorized)
    end.


get_auth_() -> get('$exodm_auth').

del_auth_() ->
    case is_sticky() of
        true ->
            true;
        false ->
            erase('$exodm_auth')
    end.

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
    exodm_db_user:subscribe(delete),
    exodm_db_account:subscribe(delete),
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
handle_call({make_user_active, U, Db}, _, St) ->
    case ets:lookup(?TAB, U) of
        [] ->
            kvdb:in_transaction(
              Db, fun(_) ->
                          case first_auth_(U, no_password) of
                              false ->
                                  {reply, false, St};
                              {true, Hash, undefined} ->
                                  #session{aid = AID, role = Role} =
                                      create_session(U, Hash, undefined),
                                  {reply, {true, AID, Role}, St}
                          end
                  end);
        [#session{aid = AID, role = Role} = Session] ->
            reset_timer(Session),
            {reply, {true, AID, Role}, St}
    end;
handle_call({remove_user_session, _Auth, UserToRemove}, _, St) ->
    %% Should only be called when removing a user
    %% Do we need to check authorization here ??
    ets:delete(?TAB, UserToRemove),
    {reply, true, St};
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
    {reply, true, St};
handle_call({remove_account, AID}, _From, St) ->
    Recs = ets:select(?TAB, [{#session{aid = AID, _ = '_'}, [], ['$_']}], 100),
    remove_account_recs(Recs),
    {reply, ok, St}.

remove_account_recs('$end_of_table') ->
    done;
remove_account_recs({Recs, Cont}) ->
    lists:foreach(
      fun(#session{user = U, timer = TRef}) ->
              if TRef =/= undefined ->  % necessary?
                      erlang:cancel_timer(TRef);
                 true -> ok
              end,
              ets:delete(?TAB, U)
      end, Recs),
    remove_account_recs(ets:select(Cont)).



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
handle_info({exodm_db_account, delete, AID}, St) ->
    Recs = ets:select(?TAB, [{#session{aid = AID, _ = '_'}, [], ['$_']}], 100),
    remove_account_recs(Recs),
    {noreply, St};
handle_info({exodm_db_user, delete, User}, St) ->
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
    ?debug("user ~p, session ~p", [User, S]),
    %% [{_, AID}] = exodm_db_user:lookup_attr(User,<<"__aid">>),
    [{AID,Role}|_] = Access = exodm_db_user:list_access(User),
    S#session{aid = exodm_db:account_id_num(AID),
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
