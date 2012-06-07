%% @author Tony Rogvall tony@rogvall.se
%% @copyright YYYY Tony Rogvall.

-module(exodm_web_group).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("gettext/include/gettext.hrl").

-export([main/0
         , title/0
         , layout/0
	 , event/1
	]).

-export([row_selected/2]).
-export([format_row/1]).

-import(lists, [map/2]).


main() ->
    exodm_web_common:content_type_html(),
    case wf:role(managers) of
	true ->
	    File = filename:join([code:priv_dir(exodm_web),
				  "templates","grid.html"]),
	    #template { file=File };
	false ->
	    wf:redirect_to_login("/login")
    end.


title() ->
    exodm_web_common:title().

layout() ->
    #container_12 {
        body=[#grid_12 { class=header, body=exodm_web_common:header(group) },
              #grid_clear {},
              #grid_6 { alpha=true, body=group_table()  },
              #grid_6 { omega=true, class=dialog, body=group_dialog() },
              #grid_clear {},
              #grid_12 { body=exodm_web_common:footer() }
             ]}.

format_row(header) ->
    [?TXT("ID"),?TXT("Name"),?TXT("Url")];
format_row(Row) ->
    [{id,proplists:get_value(id,Row,"?")},
     {name,proplists:get_value(name,Row,"?")},
     {url,proplists:get_value(url,Row,"")}
    ].


group_table() ->
    wf:state(current_id, undefined),
    AID = wf:session(account_id),
    Key = exodm_db:kvdb_key_join([exodm_db:account_id_key(AID), <<"groups">>]),
    exodm_web_table:layout(group, Key, ?MODULE).

%% Dialog elements 
group_id(Value) ->
    #textbox { id=group_id, text=Value, 
	       next=group_name,
	       postback={update,group_id} }.

group_name(Value) ->
    #textbox { id=group_name, text=Value, 
	       next=group_url,
	       postback={update,group_name} }.

group_url(Value) ->
    #textbox { id=group_url, text=Value, 
	       postback={update,group_url} }.


%% Dialog, panel is used to wrap elements for update
group_dialog() ->
    [
     #label { text=?TXT("ID")},
     group_id(""),
     
     #p{},
     #label { text=?TXT("Name")},
     group_name(""),
     #grid_clear {},
     #panel { body=[
		    #label { text=?TXT("Url") },
		    group_url("")
		   ]},
     #grid_1 { alpha=true, body=
		   #button { id=but_update, text=?TXT("Update"), 
			     postback=update }
	     },
     #grid_1 { body=#button { id=but_delete, text=?TXT("Delete"),
			      postback=delete }},

     #grid_1 { omega=true, body=
		   #button { id=but_add,    text=?TXT("Add"), 
			     postback=add }
	     },
     #grid_clear {}
    ].

fill_dialog(ID,Name,Url) ->
    wf:set(group_id, if ID =:= undefined -> ""; true -> ID end),
    wf:set(group_name, Name),
    wf:set(group_url, Url).

clear_dialog() ->
    wf:state(current_id, undefined),
    fill_dialog(undefined, "", "").

%% callback from exodm_web_table - when selected
row_selected(_I, ID) ->
    io:format("row_selected: row=~w, id=~p\n", [_I, ID]),
    GID = exodm_db:group_id_key(ID),
    AID = wf:session(account_id),
    case exodm_db_group:lookup(AID, GID) of
	[] ->
	    io:format("could not find group (~w, ~w)\n", [AID,GID]),
	    ok;
	GroupData ->
	    Name = proplists:get_value(name,GroupData,<<"">>),
	    Url  = proplists:get_value(url,GroupData,<<"">>),
	    wf:state(current_id, GID),
	    fill_dialog(GID,Name,Url)
    end.


%% event({row_clicked,I,ID}) ->
%%    ok;
event(delete) ->
    delete_current();
event(add) ->
    add_or_update(add);
event(update) ->
    add_or_update(update);
event(_Event) ->
    io:format("Event=~p~n",[_Event]),
    ok.

add_or_update(_Operation) ->
    ID   = list_to_integer(wf:q(group_id)),
    Name = wf:q(group_name),
    Url  = wf:q(group_url),
    
    io:format("~p: ID:~p, Name:~p, Url:~p\n", 
	      [_Operation,ID,Name,Url]),

    case wf:state(current_id) of
	ID ->
	    AID = wf:session(account_id),
	    case exodm_db_group:exist(AID, ID) of
		true ->
		    exodm_db_group:update(AID, ID,
					  [{name,Name},{url,Url}]);
		false ->
		    throw({invalid,id,"update error"})
	    end;
	_ -> %% no group was loaded
	    AID = wf:session(account_id),
	    case exodm_db_group:exist(AID, ID) of
		true ->
		    exodm_db_group:update(AID, ID,
					  [{name,Name},
					   {url,Url}]),
		    wf:state(current_id, undefined);
		false ->
		    exodm_db_group:new(AID, ID,
				       [{name,Name},
					{url,Url}]),
		    wf:state(current_id, undefined);
		_Error ->
		    throw({invalid,id,"update error"})
	    end
    end.

delete_current() ->
    %% Delete current record.
    %% 1. delete from database
    case wf:state(current_id) of
	undefined ->
	    ok;  %% nothing to do
	ID ->
	    AID = wf:session(account_id),
	    io:format("DELETE: (~w,~w)\n", [AID,ID]),
	    case exodm_db_group:exist(AID,ID) of
		true ->
		    %% confirm dialog? if modified? FIXME!!!
		    exodm_db_group:delete(AID,ID),
		    exodm_web_table:delete_selected(group),
		    clear_dialog();
		false ->
		    wf:state(current_id, undefined),
		    ok
	    end
    end.
