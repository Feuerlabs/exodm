%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
-module(exodm_db_session).

-behaviour(gen_server).

-export([authenticate/3, authenticate/4,
         logout/0,  logout/1,
         is_active/0, is_active/1, 
         refresh/0]).
-export([get_user/0, 
         %%get_role/0, 
         get_aid/0, 
         get_auth/0]).
-export([remove_user_session/1]).
-export([spawn_child/1, spawn_link_child/1, spawn_monitor_child/1]).

-export([remove_account/1]).

-export([set_auth_as_user/2, set_auth_as_user/3, set_auth_as_user/4,
         set_auth_as_account/2, set_auth_as_account/3, set_auth_as_account/4,
         set_aid/2,
         set_auth_as_device/1,
         identify_trusted_process/2, identify_trusted_process/3,
         set_trusted_proc/0, unset_trusted_proc/0,
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
                  hash,
                  sha,
                  timer}).

-define(INACTIVITY_TIMER, 20*60000).  % should be configurable?

-include("exodm_db.hrl").
-include_lib("lager/include/log.hrl").
-define(dbg(F,A), ?debug("~p " ++ F, [self()|A])).

%% @spec authenticate(Username, Password) -> {true, AID} | false
authenticate(AID, User, Pwd) ->
    authenticate(user, AID, User, Pwd).

authenticate(Type, AID, ID0, Pwd) ->
    ?debug("type = ~p, aid = ~p, id ~p~n", [Type, AID, ID0]),
    ID = session_id(Type, AID, to_binary(ID0)),
    case gen_server:call(
           ?MODULE, {auth, AID, ID, to_binary(Pwd)}, 10000) of
        true ->
            put_auth_({ID, AID, undefined}),
            true;
        false ->
            false
    end.

session_id(device, AID, {did,AID,_} = ID) -> ID;
session_id(device, AID, ID) -> {did, AID, ID};
session_id(user  , _  , ID) -> ID.

remove_account(AID) ->
    gen_server:call(?MODULE, {remove_account, AID}).

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
                    {did,_,_} -> User;
                    _ -> to_binary(User)
                end,
            ets:member(?TAB, K)
    end.

set_trusted_proc() ->
    put('$exodm_trusted_proc', true),
    ok.

set_trusted_proc(true) -> set_trusted_proc();
set_trusted_proc(false) -> ok.

unset_trusted_proc() ->
    erase('$exodm_trusted_proc').

is_trusted_proc() ->
    get('$exodm_trusted_proc') == true.

%% This is ugly !! FIXME
set_aid(Aid, User) ->
    case ets:lookup(?TAB, User) of
        [] -> 
            error(no_user_session);
        [#session{role = Role}] ->
            put_auth_({User, Aid, Role}),
            ets:update_element(?TAB, User, {#session.aid, Aid})
    end.
            
%% set_role(Aid, User, Role) ->
%%     case ets:lookup(?TAB, User) of
%%         [] -> 
%%             error(no_user_session);
%%         [S] ->
%%             put_auth_({User, Aid, Role}),
%%             ets:update_element(?TAB, User, {#session.role, Role})
%%     end.
            

set_auth_as_user(Aid, User) ->
    set_auth_as_user(Aid, User, kvdb_conf, false).

set_auth_as_user(Aid, User, Db) ->
    set_auth_as_user(Aid, User, Db, false).

set_auth_as_user(Aid, User, Db, Sticky) ->
    ?debug("aid ~p, user ~p", [Aid, User]),
    case ets:lookup(?TAB, User) of
        [] ->
            ?debug("new user", []),
            case gen_server:call(?MODULE,
                                 {make_user_active, 
                                  to_binary(Aid), 
                                  to_binary(User), 
                                  Db}) of
                true ->
                    ?debug("authorized", []),
                    put_auth_({User, Aid, undefined}),
                    set_sticky_flag(Sticky),
                    true;
                false ->
                    ?debug("not authorized", []),
                    false
            end;
        [#session{role = Role}] ->
            ?debug("old user", []),
            put_auth_({User, Aid, Role}),
            ets:update_element(?TAB, User, {#session.aid, Aid}),
            set_sticky_flag(Sticky),
            true
    end.

identify_trusted_process(Aid, User) ->
    identify_trusted_process(Aid, User, kvdb_conf).

identify_trusted_process(Aid, User, Db) ->
    case lookup_user_info(Db, Aid, User, no_auth) of
        #session{role = Role} ->
            put_auth_({User, Aid, Role}),
            set_trusted_proc(),
            ok;
        false ->
            error({cannot_identify_as, {Aid, User}})
    end.

set_auth_as_account(Account, User) when is_binary(Account) ->
    set_auth_as_account(Account, User, kvdb_conf, false).

set_auth_as_account(Account, User, Db) when is_binary(Account) ->
    set_auth_as_account(Account, User, Db, false).

set_auth_as_account(Account, User, Db, Sticky) when is_binary(Account) ->
    case exodm_db_account:lookup_by_name(Account) of
        Aid when is_binary(Aid) ->  
            set_auth_as_user(Aid, User, Db, Sticky);
        false -> 
            ?error("Account ~p missing!!", [Account]), 
            false
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
    put_auth_({session_id(device, AID, DID), AID, undefined}),
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
%% get_role() -> if_active_(get_auth_(), fun({_,_,X}) -> X end).
get_auth() -> if_active_(get_auth_(), fun(X) -> X end).

if_active_({{did,_,_},_,_} = X, Ret) ->
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
    Auth = get_auth_(),  % skip if_active check
    Trusted = is_trusted_proc(),
    fun() ->
            put_auth_(Auth),
            set_trusted_proc(Trusted),
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

handle_call({auth, A, ID, P}, From, St) ->
    ?debug("aid = ~p, id = ~p", [A, ID]),
    case ets:lookup(?TAB, ID) of
	[] ->
            ?debug("not in table", []),
	    {noreply, pending(A, ID, [{From, P}], St)};
        [#session{sha = undefined}] ->
            ?debug("sha undefined", []),
            %% system processes have accessed user, but not authenticated
            %% sessions.
            {noreply, pending(A, ID, [{From,P}], St)};
	[#session{hash = Hash, sha = Sha} = Session] ->
           ?debug("sha defined", []),
	    case sha(Hash, P) of
		Sha ->
                    ?debug("sha asserted", []),
		    reset_timer(Session),
		    {reply, true, St};
		_ ->
                    ?debug("sha assertion failed", []),
		    {reply, false, St}
	    end
    end;
handle_call({make_user_active, A, U, Db}, _, St) ->
    case ets:lookup(?TAB, U) of
        [] ->
            try lookup_user_info(Db, A, U, no_password) of
                #session{} = Session ->
                    create_session(Session),
                    {reply, true, St};
                false ->
                    {reply, false, St}
            catch error:R ->
                    io:fwrite("ERROR ~p~n~p~n", [R,erlang:get_get_stacktrace()]),
                    {reply, false, St}
            end;
        [#session{} = Session] ->
            reset_timer(Session),
            {reply, true, St}
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



handle_cast({first_auth, Pid, Aid, User, Res}, #st{pending = Pend,
					      procs = Procs} = St) ->
    Waiting = dict:fetch(User, Pend),
    St1 = St#st{pending = dict:erase(User, Pend),
		procs = dict:erase(Pid, Procs)},
    case Res of
        #session{} = Session ->
            create_session(Session),
	    {noreply, process_pending(true, Waiting, Aid, User, St1)};
	false ->
	    {noreply, process_pending(false, Waiting, Aid, User, St1)}
    end.

handle_info({timeout, TRef, User}, St) ->
    ?debug("Session timeout (~s)~n", [User]),
    case ets:lookup(?TAB, User) of
        [#session{timer = TRef}] ->
            ets:delete(?TAB, User);
        _ ->
            ?debug("Timer ref (~p) doesn't match session ~s", [TRef, User])
    end,
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

process_pending(Reply, Waiting, Aid, User, St) ->
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
		    pending(Aid, User, Different, St)
	    end
    end.

pending(A, ID, [{_, P}|_] = Clients, #st{pending = Pend, procs = Procs} = St) ->
    ?debug("id ~p, pending ~p", [ID, Pend]),
    case dict:is_key(ID, Pend) of
	true ->
            ?debug("is key", []),
	    St#st{pending = dict:append_list(ID, Clients, Pend)};
	false ->
            ?debug("is not key", []),
            Me = self(),
            {Pid,_} = spawn_monitor(fun() ->
                                            first_auth(A, ID, P, Me)
                                    end),
            St#st{pending = dict:store(ID, Clients, Pend),
                  procs = dict:store(Pid, ID, Procs)}
    end.

first_auth(A, ID, P, Parent) ->
    Res = lookup_user_info(kvdb_conf, A, ID, P),
    %% Res = first_auth_(A, ID, P),
    gen_server:cast(Parent, {first_auth, self(), A, ID, Res}).

first_auth_(A, ID, P) ->
    {Type, LookupRes} =
        case ID of
            {did, AID, DID} ->
                {device, exodm_db_device:lookup_attr(
                           AID, DID, ?DEV_DB_PASSWORD)};
            _ ->
                {user, exodm_db_user:lookup_attr(ID, password)}
        end,
    case LookupRes of
        [] ->
            ?debug("id ~p, no hash", [ID]),
            case ID of
                {did, AID1, DID1} ->
                    ?debug("allow device ~p with no password~n", [ID]),
                    #session{user = session_id(device, AID1, DID1),
                             aid = AID1};
                _ ->
                    false
            end;
        [{_, Hash}] ->
            ?debug("id ~p, hash ~p", [ID, Hash]),
            case P of
                no_password ->
                    %% means we're faking authentication of a system process
                    %% we can't make a sha hash, since we don't know the pwd.
                    ?debug("id ~p, no password ", [ID]),
                    #session{user = session_id(Type, A, ID),
                             aid = A,
                             hash = Hash};
                _ when is_binary(P) ->
                    ?debug("id ~p, password ********", [ID]),
                    {ok, HashStr} = bcrypt:hashpw(P, Hash),
                    ?debug("id ~p, new hash ~p ", [ID, HashStr]),
                    case list_to_binary(HashStr) of
                        Hash ->
                            ?debug("bcrypt hash matches for ~p~n", [ID]),
                            #session{user = session_id(Type, A, ID),
                                     aid = A,
                                     hash = Hash,
                                     sha = sha(Hash, P)};
                        _ ->
                            ?debug("bcrypt hash doesn't match for ~p~n"
                                   "~p | ~p~n", [ID, HashStr, Hash]),
                            false
                    end
            end
    end.

%% create_session(AID, User, Hash, Sha) ->
%%     ?debug("aid ~p, user ~p", [AID, User]),
%%     %% FIXME
%%     %% User can have several roles !!
%%     %% Role = max_role(exodm_db_user:list_account_roles(AID, User)),

%%     create_session(#session{user = User,
%%                             aid = AID,
%%                             hash = Hash,
%%                             sha = Sha}).

create_session(#session{user = User, sha = Sha} = S) ->
    Session =
        case ets:insert_new(?TAB, S) of
            false ->
                case ets:lookup(?TAB, User) of
                    [S0] ->
                        ?debug("Tried creating session twice:~nS0 = ~p~n"
                               "S = ~p~n", [S0, S]),
                        if S0#session.sha == undefined ->
                                %% Use update_element to avoid lost update issues
                                ets:update_element(?TAB, User,
                                                   {#session.sha, Sha}),
                                S0#session{sha = Sha};
                           true ->
                                S0
                        end;
                    [] ->
                        ets:insert(?TAB, S),
                        S
                end;
            true ->
                S
        end,
    if Session#session.timer == undefined ->
            TRef = start_timer(User),
            ets:update_element(?TAB, User, {#session.timer, TRef}),
            Session#session{timer = TRef};
       true ->
            Session
    end.

%% max_role([Role]) ->
%%     Role;
%% %% FIXME !!!
%% max_role([Role | _]) ->
%%     Role.


sha(Hash, Passwd) ->
    crypto:sha_mac(Hash, Passwd).

start_timer(User) ->
    erlang:start_timer(inactivity_timer(), self(), User).

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

inactivity_timer() ->
    case application:get_env(exodm, user_session_timeout) of
        {ok, T} when is_integer(T), T > 0 ->
            T;
        _ ->
            ?INACTIVITY_TIMER
    end.

%% lookup_user_info(Db, Aid, User) ->
%%     lookup_user_info(Db, Aid, User, no_auth).

lookup_user_info(Db0, A, U, Auth) ->
    kvdb:in_transaction(
      Db0, fun(Db) ->
                   %% Check that the user has access to account
                   case list_accounts(U) of
                       [] ->
                           ?debug("No accounts for user ~s~n", [U]),
                           ?debug("User record: ~p~n",
                                  [exodm_db_user:lookup(U)]),
                           false;
                       AList ->
                           case lists:member(A, AList) of
                               true ->
                                   case Auth of
                                       no_auth ->
                                           #session{user = U,
                                                    aid = A};
                                       _ ->
                                           first_auth_(A, U, Auth)
                                   end;
                               false ->
                                   ?debug("Account ~s not member of ~p~n",
                                          [A, AList]),
                                   false
                           end
                   end
           end).

list_accounts({did, AID, ID}) ->
    [AID];
list_accounts(User) ->
    exodm_db_user:list_accounts(User).
