#!/usr/bin/env escript
%% -*- erlang -*-

%% Copyright 2011-13 Feuerlabs Inc.
%% License: "BSD New" (http://www.opensource.org/licenses/BSD-3-Clause)
%% Author: Ulf Wiger <ulf@feuerlabs.com>

-record(state, {capture = [beam],
		diff_prev = " HEAD^",
		diff_cur  = "",
		diff_tail = [],
		compress = false,
		output = tty}).

main(Args) ->
    try begin
            St = args(Args, #state{}),
            run_diff(St)
        end
    catch
        error:E ->
            rpt_error(E, erlang:get_stacktrace()),
            usage(),
            halt(1)
    end.

run_diff(#state{diff_prev = Prev, diff_cur = Cur, diff_tail = Tail,
		capture = Capture, output = _Target} = St) ->
    Cmd = "git diff" ++ Prev ++ Cur ++ " --name-only" ++ Tail,
    io:fwrite("~s~n", [Cmd]),
    DiffResult = re:split(os:cmd(Cmd), "\\v", [{return, list}]),
    Classified = [{file_type(F), F} || F <- DiffResult, F=/=[]],
    case [F || {unknown, F} <- Classified] of
	[] ->
	    Grouped = group(Classified),
	    Keep = [{Type, [actual_file(Type, F) || F <- Fs]}
		    || {Type, Fs} <- Grouped, lists:member(Type, Capture)],
	    output(Keep, St);
	[_|_] = Unknown ->
	    io:fwrite("Unknown = ~p~n", [Unknown]),
	    io:fwrite("Can't recognize:~n", []), [io:fwrite("  ~s~n", [F])
						  || F <- Unknown],
	    halt(1)
    end.

group(List) ->
    lists:foldl(fun({T,F},D) -> orddict:append(T,F,D) end, orddict:new(), List).


file_type(F) when F=="Makefile"; F=="rebar.config"; F=="rebar.config.script";
		  F=="patch_diff.escript"; F=="rebar"; F=="devrun";
		  F=="README.md"; F=="rel/sys.config" ->
    make;
file_type(F) when F=="vm.args"; F=="ctl"; F=="make_node" ->
    setup;
file_type(F) ->
    %% use longest match to find extension, rather than filename:extension/1
    case re:run(F,"\\..+$",[{capture,all,list}]) of
	{match, [".erl"]} ->
	    beam;
	{match, [".app.src"]} ->
	    app;
	_ ->
	    unknown
    end.

actual_file(beam, F) ->
    [Base, "src" | RevTail] = lists:reverse(filename:split(F)),
    NewBase = filename:basename(Base,".erl") ++ ".beam",
    Beam = filename:join(
	     lists:reverse([NewBase, "ebin" | RevTail])),
    case filelib:is_regular(Beam) of
	true -> Beam;
	false -> error({cannot_find, Beam})
    end.

output(L, #state{output = tty}) ->
    io:fwrite("Files = ~p~n", [L]);
output(L, #state{output = {tar, F}, compress = Compress}) ->
    make_tar(L, F, Compress).

make_tar(L, Tar, Compress) ->
    Patches = "rel/patches",
    FileList =
	lists:flatmap(
	  fun({beam,Files}) ->
		  [{filename:join(Patches, filename:basename(F)), F}
		   || F <- Files]
	  end, L),
    erl_tar:create(Tar, FileList, [compressed || Compress]).

args(["-cur", V|T], St) ->
    args(T, St#state{diff_cur = " " ++ V});
args(["-prev", V|T], St) ->
    args(T, St#state{diff_prev = " " ++ V});
args(["-tar", F|T], St) ->
    args(T, St#state{output = {tar, F}});
args(["-z" | T], St) ->
    args(T, St#state{compress = true});
args(["-capture" | T], St) ->
    C = lists:partition(fun(X) ->
					lists:member(list_to_atom(X), types())
				end, T)
    args(T, St#state{
args(["--" | T], St) ->
    St#state{diff_tail = [" --"| [[" ",X] || X <- T]]};
args([H|_], _St) ->
    error({?MODULE, unknown_argument, H});
args([], St) ->
    St.

types() ->
    [beam, app, all].

usage() ->
    Script = escript:script_name(),
    Pad = lists:duplicate(length(Script) +7, $\s), % "Usage: "
    io:fwrite("You're doing it wrong!~n"
	      "Usage: ~s" ++
		  " [-prev PrevVsn] [-z] [-tar TarFile]~n" ++ Pad ++
		  " [-- Path]~n",
	      [escript:script_name()]).

rpt_error({?MODULE, What, Info}, _) ->
    io:fwrite("ERROR: ~p~n", [{What, Info}]);
rpt_error(E, Stack) ->
    io:fwrite("ERROR: ~p~n"
              "Stack: ~p~n", [E, Stack]).
