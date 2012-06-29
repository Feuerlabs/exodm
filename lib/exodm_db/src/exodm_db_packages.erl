%% @author Tony Rogvall <tony@feuerlabs.com>
%%% @copyright (C) 2012, Feuerlabs, Inc.
%%% @doc
%%%     Store and Manage package files
%%%     Theory of operations. Packages are stored in a tree structure
%%%     organized by name version and architecture (in some levels)
%%%
%%%     /packages/<Package>/<Version>/<Architecture>/
%%%     /<aid>/packages/<Package>/<Version>/<Architecture>/
%%%     /<aid>/devices/<did>/packages/<Package>/<Version>/<Architecture>/
%%%
%%%           raw       -- the orginal packet
%%%           control/  -- unpacked meta data
%%%                 Description
%%%                 Architecture
%%%                 Provides
%%%                 Depends
%%%
%%% @end
%%% Created : 31 May 2012 by Tony Rogvall <tony@feuerlabs.com>

-module(exodm_db_packages).

-export([new/1, new/2]).
-export([load_directory/1, load_directory/2]).
-export([load_file/1, load_file/2]).
-export([list_packages/0, list_packages/1]).
-compile(export_all).

-type vsn_operator() ::  '<<' | '<=' | '=' | '>=' | '>>'.
-type arch_operator() :: '!'.
-type vsn_pattern() :: {vsn_operator(), version()}.
-type arch_pattern() :: {arch_operator(),architecture()} | architecture().
-type version() :: {vsn_epoch(),vsn_version(),vsn_revision()}.
-type architecture() :: 'any' | {arch_os(),'any'} | {'any',arch_cpu()} |
			{arch_os(),arch_cpu()}.
-type arch_os() :: string().
-type arch_cpu() :: string().
-type vsn_epoch() :: non_neg_integer().
-type vsn_version() :: string().
-type vsn_revision() :: string().

%% select packages
-record(opkg,
	{
	  package :: binary(),  %% name of package
	  version :: version(),
	  arch    :: architecture(),
	  key     :: binary()   %% raw key to the child
	}).

%% from dependencies
-record(opkg_pattern,
	{
	  package :: binary(),  %% name of package
	  version :: vsn_pattern(),
	  arch    :: arch_pattern()
	}).

-import(lists, [reverse/1]).


	    

new(PackageFile) ->
    new(<<"packages">>, PackageFile).

new(ParentKey, PackageFile) ->
    load_file(ParentKey, PackageFile).

%% Load all packages in the directory Dir
load_directory(Dir) ->
    load_directory(<<"packages">>, Dir).

load_directory(ParentKey, Dir) ->
    case file:list_dir(Dir) of
	{ok,Fs} ->
	    lists:map(
	      fun(F) -> 
		      load_file(ParentKey, filename:join(Dir, F))
	      end, Fs);
	Error ->
	    Error
    end.

load_file(File) ->
    load_file(<<"packages">>, File).

load_file(ParentKey, File) ->
    case erl_opkg:get_control(File) of
	{ok, CData} ->
	    Package = proplists:get_value(<<"Package">>, CData),
	    Version = proplists:get_value(<<"Version">>, CData),
	    Arch = proplists:get_value(<<"Architecture">>, CData),
	    Key = exodm_db:kvdb_key_join([ParentKey,Package,Version,Arch]),
	    DKey = exodm_db:kvdb_key_join([Key,<<"data">>]),
	    CKey = exodm_db:kvdb_key_join([Key,<<"control">>]),
	    {ok,Bin} = file:read_file(File),
	    exodm_db:write(DKey, Bin),
	    io:format("load_file: ~p - ~p - ~p\n", [Package,Version,Arch]),
	    lists:foreach(
	      fun({Attr,Value}) ->
		      CKey1 = exodm_db:kvdb_key_join([CKey,Attr]),
		      exodm_db:write(CKey1, Value)
	      end, CData),
	    [] /= exodm_db:all_children(Key);
	Error ->
	    io:format("load_file: error ~s (~p)\n", [File, Error]),
	    Error
    end.


all_versions(Package) when is_binary(Package) ->
    fold_version(fun(P, Acc) ->  [P|Acc] end, [], Package).

%%
%% Fold over all version-arch combination for a given package
%% Fun is called with arguments Fun({Package,Vsn,Arch}, Acc) 
%% Vsn = {Epoch,Version,Revision}  (use erl_opkg:compare_version!!!)
%% Arch = any | {Os,Cpu}
%%
fold_version(Fun, Acc, Package) ->
    exodm_db:fold_children(
      fun(VersionChild,Acc0) ->
	      exodm_db:fold_children(
		fun(ArchChild,Acc1) ->
			[A,V|_]=reverse(exodm_db:kvdb_key_split(ArchChild)),
			{Vsn,""}=erl_opkg:parse_version(binary_to_list(V)),
			{Arc,""}=erl_opkg:parse_architecture(binary_to_list(A)),
			Pat = #opkg { package=Package,
				      version=Vsn,
				      arch=Arc,
				      key=ArchChild},
			Fun(Pat,Acc1)
		end, Acc0, VersionChild)
      end, Acc, <<"packages*",Package/binary >>).

%%
%% Get the latest version - ArchPattern {'!',<arch>} or <arch>
%% <arc> = any | {any,<cpu>} | {<os>,any} | {<os>,<cpu>}
%%
latest_version(Package, ArchPattern) ->
    PVAs = select_architecture(Package, ArchPattern),
    [Latest|_] = sort_package_versions(PVAs),
    Latest.

%%
%% Select version matching architecture - Arch {'!',<arch>} or <arch>
%% <arc> = any | {any,<cpu>} | {<os>,any} | {<os>,<cpu>}
%%
select_architecture(Package, ArchPattern) ->
    fold_version(
      fun(P,Acc) ->
	      case is_architecture(ArchPattern, P#opkg.arch) of
		  true -> [P|Acc];
		  false -> Acc
	      end
      end, [], Package).


%% Apply both Architecture pattern and Version Pattern to select
%% packages
select_version(#opkg_pattern{package=Package, version=Version, arch=Arch}) ->
    select_version(Package, Version, Arch).

select_version(Package, latest, ArchPattern) ->
    %% should use default arch some how (how is done in debian?)
    PVAs = select_architecture(Package, ArchPattern),
    case sort_package_versions(PVAs) of
	[Latest|_] -> [Latest];
	[] -> []
    end;
select_version(Package, {VersionOp,VersionPattern}, ArchPattern) ->
    lists:filter(
      fun(P) ->
	      Cmp = erl_opkg:compare_version(VersionPattern,P#opkg.version),
	      test_version(VersionOp, Cmp)
      end, select_architecture(Package, ArchPattern)).

%% sort candiates according to version {Package,Vsn,Arch}
sort_package_versions(PVAs) ->
    lists:sort(fun(P1,P2) ->
		       erl_opkg:compare_version(P1#opkg.version,
						P2#opkg.version) =< 0
	       end, PVAs).

depends(#opkg { package=Package,version=Version,arch=Arch}) ->
    depends(Package, format_version(Version), format_arch(Arch)).
    
%% Get dependencies
depends(Package,Version,Arch) ->
    case exodm_db:read(<<"packages", "*", 
			 Package/binary, "*",
			 Version/binary, "*",
			 Arch/binary, "*"
			 "control*Depends">>) of
	{ok,{_Key, _Attr, Value}} ->
	    Res = erl_opkg:parse_depends(binary_to_list(Value)),
	    io:format("depends: ~s ~s ~s = ~p\n", [Package,Version,Arch,Res]),
	    Res;
	Error ->
	    Error
    end.

%% build a list of all package needed for installing
%% the packge <<"Package">> of version <<"Version">> 

%%
%% example resolve(<<"erlang">>, <<"R12B">>, <<"x86">>).
%%
resolve(P, V, A) when is_binary(P), is_binary(V), is_binary(A) ->
    {Vsn,""}=erl_opkg:parse_version(binary_to_list(V)),
    {Arc,""}=erl_opkg:parse_architecture(binary_to_list(A)),    
    Opkg = #opkg { package = P, version=Vsn, arch=Arc },
    case depends(P, V, A) of
	Err = {error,_} -> Err;
	Deps -> 
	    resolve_or(Deps, Arc, package_set_new([Opkg]))
    end.

%% resolve alternatives (fixme - backtrack)
resolve_or([Alt | Alts], Arch0, Set) ->
    case resolve_and(Alt, Arch0, Set) of
	Err = {error,_} -> Err;
	empty -> resolve_or(Alts, Arch0, Set);
	Set1 -> Set1
    end;
resolve_or([], _Arch, _Set) ->
    empty.

%% resolve all packages in a list
resolve_and([X={Package,VersionOpPatern,ArchPattern}|Ps], Arch0, Set) ->
    io:format("Check package: ~p\n", [X]),
    case select_version(list_to_binary(Package),VersionOpPatern,ArchPattern) of
	[] -> 
	    empty;  %% dependency is missing
	PVAs ->
	    %% check dependencies
	    Set1=
		lists:foldl(
		  fun(P,Set0) ->
			  case package_set_is_member(P, Set0) of
			      true -> 
				  Set0;
			      false ->
				  case depends(P) of
				      {error,_} ->
					  Set0;
				      [] -> 
					  [P|Set0];  %% no dependencies! ok!
				      PAlts ->
					  case resolve_or(PAlts,Arch0,Set0) of
					      empty -> Set0;
					      {error,_} -> Set0;
					      Set1 -> Set1
					  end
				  end
			  end
		  end, Set, PVAs),
		resolve_and(Ps, Arch0, Set1)
	end;
resolve_and([], _ArchPattern, Set) ->
    Set.


    
package_set_new() -> [].
package_set_new(List) -> List.

package_set_is_member(Pkg, [H|T]) ->
    if Pkg#opkg.package =:= H#opkg.package,
       Pkg#opkg.version =:= H#opkg.version,
       Pkg#opkg.arch =:= H#opkg.arch ->
	    true;
       true ->
	    package_set_is_member(Pkg, T)
    end;
package_set_is_member(_Pkg, []) ->
    false.

%% check if arcitecture pattern match
is_architecture({'!',A}, A1) ->  not is_architecture(A, A1);
is_architecture(A, A) -> true;
is_architecture(any, _A) -> true;
is_architecture({any,Cpu},{_,Cpu}) -> true;
is_architecture({Os,any},{Os,_})   -> true;
is_architecture(_, _) -> false.


test_version('<<', Cmp) -> Cmp < 0;
test_version('<=', Cmp) -> Cmp =< 0;
test_version('=',  Cmp) -> Cmp =:= 0;
test_version('>=', Cmp) -> Cmp >= 0;
test_version('>>', Cmp) -> Cmp > 0.

    
format_version({0,Vsn,""}) ->
    list_to_binary(Vsn);
format_version({0,Vsn,Rev}) ->
    << (list_to_binary(Vsn))/binary, "-", (list_to_binary(Rev))/binary>>;
format_version({E,Vsn,""}) ->
    << (list_to_binary(integer_to_list(E)))/binary, ":",
       (list_to_binary(Vsn))/binary >>;
format_version({E,Vsn,Rev}) ->
    << (list_to_binary(integer_to_list(E)))/binary, ":",
       (list_to_binary(Vsn))/binary, "-", (list_to_binary(Rev))/binary>>.

format_arch(any) -> <<>>;
format_arch({any,Cpu}) -> list_to_binary(Cpu);
format_arch({Os,any}) ->  list_to_binary(Os);
format_arch({Os,Cpu}) -> 
    << (list_to_binary(Os))/binary, "-", (list_to_binary(Cpu))/binary>>.

list_packages() ->
    list_packages("").

list_packages(Match) ->
    exodm_db:fold_children(
      fun(K,_Acc) -> list_version_(K,Match) end, ok, <<"packages">>).

list_version_(K,Match) ->
    exodm_db:fold_children(
      fun(Kv,_Acc) -> list_arch_(Kv,Match) end, ok, K).      

list_arch_(K,Match) ->
    exodm_db:fold_children(
      fun(Ka,_Acc) -> list_pack_(Ka,Match) end, ok, K).

list_pack_(K,Match) ->
    Ctl = exodm_db:kvdb_key_join([K,<<"control">>]),
    {ok,{_K1,_A1,Name}} = 
	exodm_db:read(exodm_db:kvdb_key_join(Ctl,<<"Package">>)),
    case re:run(Name, ".*"++Match++".*") of
	nomatch -> ok;
	{match,_} ->
	    {ok,{_K2,_A2,Vers}} = 
		exodm_db:read(exodm_db:kvdb_key_join(Ctl,<<"Version">>)),
	    {ok,{_K3,_A3,Arch}} = 
		exodm_db:read(exodm_db:kvdb_key_join(Ctl,<<"Architecture">>)),
	    io:format("~s-~s-~s\n", [Name,Vers,Arch])
    end.

			      



