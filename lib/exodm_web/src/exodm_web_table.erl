%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2010, Tony Rogvall
%%% @doc
%%%   Table handler
%%% @end
%%% Created :  6 Sep 2010 by Tony Rogvall <tony@rogvall.se>

-module(exodm_web_table).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("gettext/include/gettext.hrl").

-export([layout/4, layout/5, layout/6]).
-export([event/1]).

-compile(export_all).

-import(lists, [map/2, reverse/1]).

row_id(Table, I) ->
    atom_to_list(Table)++"_row_"++integer_to_list(I).

layout(Type, Tab, Key, Module) ->
    layout(Type, Tab, Key, 10, Module).

layout(Type, Tab, Key, N, Module) ->
    layout(Type, Tab, Key, N, exodm_db:first_child(Tab, Key), Module).

layout(Type, Tab, _ParentKey, N, StartKey, Module) ->
    Hs = Module:format_row(header),
    THCells = [#tableheader { text=Text} || Text <- Hs],
    Header = #tablerow { cells=THCells },
    {_,Rows} = 
	lists:foldr(
	  fun(Rec,{I,Acc}) ->
		  TR = create_table_row(Type, Module, I, Rec),
		  {I+1, [TR | Acc]}
	  end, {1,[]}, fetch_records(Type, Tab,StartKey,N)),
    Prev = #event { type=click, delegate=?MODULE, postback={prev_page,Type}},
    Next = #event { type=click, delegate=?MODULE, postback={next_page,Type}},
    PrevCell = #tablecell { valign=top, align=left,
			    body=#image { id=table_prev,
					  actions = Prev,
					  image = "images/prev_page.png"}},
    NextCell = #tablecell { valign=top, align=right,
			    body=#image { id=table_next,
					  actions = Next,
					  image = "images/next_page.png"}},
    PadCells = [#tablecell { html_encode=false, text="&nbsp;" } || _ <- lists:seq(1, length(Hs)-2)],
    Footer = #tablerow { cells= [PrevCell] ++ PadCells ++ [NextCell]},
    #table { class=exo_table, id=exotable, 
	      style="width: 100%;", 
	      header=Header,
	      rows = Rows,
	      footer = Footer }.


%% Create a table cell from a exosense record
create_table_row(Type, Module, I, empty) ->
    N = length(Module:format_row(header)),
    Cells=[#tableheader { html_encode=false, text="&nbsp;"} |
	   [#tablecell { html_encode=false, text="&nbsp;" } ||
	       _ <- lists:seq(1, N-1)]],
    Class = element((I band 1)+1, {"","odd"}),
    RowID = row_id(Type, I),
    #tablerow { id=RowID, class=Class, cells=Cells };    
create_table_row(Type, Module, I, Rec) ->
    [{id,ID}|Hs] = Module:format_row(Rec),
    Cells=[#tableheader { text=ID } |
	   [#tablecell { text=Value} || {_Key,Value} <- Hs]],
    Actions = #event { type=click,
		       delegate=?MODULE,
		       postback={row_selected,Type,Module,I,ID}},
    Class = element((I band 1)+1, {"","odd"}),
    RowID = row_id(Type, I),
    #tablerow { id=RowID, class=Class, 
		cells=Cells, actions=Actions }.

fetch_records(_, _, _, 0) ->
    [];
fetch_records(Type, Tab, done, I) ->
    [empty | fetch_records(Type, Tab,done,I-1)];
fetch_records(Type, Tab, {ok,Key}, I) ->
    case read_record(Type, Key) of
	[] ->
	    fetch_records(Type, Tab, exodm_db:next_child(Tab, Key), I);
	Rec ->
	    [Rec|fetch_records(Type, Tab, exodm_db:next_child(Tab, Key), I-1)]
    end.

%% FIXME: we should pass UUID here to allow for consistency check
read_record(group, Key) ->
    exodm_db_group:lookup(Key);
read_record(device, Key) ->
    AID = wf:session(account_id),
    exodm_db_device:lookup(AID, Key).

%% read_record(device, Key) ->
%%     exodm_db_device:lookup(Key);
%% read_record(group, Key) ->
%%     exodm_db_group:lookup(Key).
	    


select_(_Table, RowID) ->
    wf:wire(RowID, #add_class { class=selected, speed=100 }),
    wf:state(row_id, RowID).

deselect_(_Table, RowID) ->
    wf:wire(RowID, #remove_class { class=selected, speed=100 }),
    wf:state(row_id, undefined).

delete_(_Table, RowID) ->
    wf:remove(RowID).

select(Table, RowID) ->
    case wf:state(row_id) of
	RowID -> %% already selected
	    ok;
	undefined ->
	    select_(Table, RowID);
	RowID1 ->
	    deselect_(Table, RowID1),
	    select_(Table, RowID)
    end.

deselect(Table) ->
    case wf:state(row_id) of
	undefined ->
	    ok; %% nothing selected
	RowID ->
	    deselect_(Table, RowID)
    end.

delete_selected(Table) ->
    case wf:state(row_id) of
	undefined ->
	    ok;  %% nothing selected
	RowID ->
	    deselect_(Table, RowID),
	    delete_(Table, RowID)
    end.
    
%% click on a table row
event({row_selected,Table,Module,I,ID}) ->
    RowID = row_id(Table, I),
    io:format("exodm_web_table: clicked ID=~s\n", [RowID]),
    io:format("            : element ID=~s\n", [ID]),
    select(Table, RowID),
    %% dialog callback - to read and fill in dialog data from table
    Module:row_selected(I, ID),
    ok;
event({next_page,_Table}) ->
    io:format("NEXT_PAGE"),
    ok;
event({prev_page,_Table}) ->
    io:format("PREV_PAGE"),
    ok;
event(_Other) ->
    %% io:format("exodm_web_table: event = ~p\n", [Other]),
    ok.

