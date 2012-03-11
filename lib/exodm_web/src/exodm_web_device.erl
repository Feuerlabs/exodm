%% @author Tony Rogvall tony@rogvall.se
%% @copyright YYYY Tony Rogvall.

-module(exodm_web_device).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("gettext/include/gettext.hrl").

-export([main/0
         , title/0
         , layout/0
	 , event/1
	]).

-import(lists, [map/2]).

%% maxweb_table callbacks
-export([row_selected/2]).
-export([format_row/1]).

-define(GA_CUSTOMER_ID, 16#00000001).
-define(CURRENT_USER, 12).

main() ->
    exodm_web_common:content_type_html(),
    File = filename:join([code:priv_dir(exodm_web),"templates","grid.html"]),
    #template { file=File }.

title() ->
    exodm_web_common:title().

layout() ->
    #container_12 {
        body=[#grid_12 { class=header, body=exodm_web_common:header(device) },
              #grid_clear {},

              #grid_6 { alpha=true, body=device_table() },
              #grid_6 { omega=true, body=
			    [#panel { class=dialog,body=device_id_entry()},
			     #br{},
			     #panel { class=dialog,body=device_dialog()}
			    ]},
              #grid_clear {},
              #grid_12 { body=exodm_web_common:footer() }
             ]}.

format_row(header) ->
    [?TXT("Device"), ?TXT("Status"), ?TXT("MSISDN")];
format_row(Row) ->
    [{id,proplists:get_value(id,Row,"?")},
     {status,proplists:get_value(status,Row,"?")},
     {msisdn,proplists:get_value(msisdn,Row,"")}
    ].

device_table() ->
    wf:state(current_id, undefined),
    U = ck3_db:user_id_key(?CURRENT_USER),
    Key = <<U/binary, "*devices">>,
    exodm_web_table:layout(device, Key, ?MODULE).

device_id_entry() ->
    wf:comet(fun() -> background_update_entry() end),
    Actions = #event { type=click, postback=copy },
    [
     #grid_clear {},
     #grid_2 { body=[#label    { text=?TXT("Activity ID")},
		     #hidden   { id=last_rfid_value },
		     #singlerow { cells=
				      [#tablecell { id=last_rfid,
						    actions=Actions,
						    html_encode=false,
						    text="&nbsp;"}]}
		    ]},
     #grid_clear {}
    ].

device_status(Value) ->
    #radiogroup { id=device_status_grp,
		  body=[
			%% This is really UGLY!!!!
			#hidden { id=device_status_value,
				  text=Value },
			#radio { id=device_status_active,
				 checked=(Value=:="active"),
				 postback={update,device_status_active},
				 text=?TXT("Active") },
			#radio { id=device_status_inactive,
				 checked=(Value=:="inactive"),
				 postback={update,device_status_inactive},
				 text=?TXT("Inacctive") },
			#radio { id=device_status_broken,
				 checked=(Value=:="broken"),
				 postback={update,device_status_broken},
				 text=?TXT("Broken") },
			#radio { id=device_status_missing,
				 checked=(Value=:="missing"),
				 postback={update,device_status_missing},
				 text=?TXT("Missing") }
		       ]}. 

device_dialog() ->
    [
     #label { text=?TXT("Device") },
     device_id(""),
     #label { text=?TXT("MSISDN") },
     device_msisdn(""),
     #p{},
     device_status(""),
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

device_id(Value) ->
    #textbox { id=device_id, text=Value,
	       postback={update,device_id} }.

device_msisdn(Value) ->
    #textbox { id=device_employee, text=Value, 
	       postback={update,device_msisdn} }.

fill_dialog(ID, Status, MSISDN) ->
    wf:set(device_id, if ID =:= undefined -> ""; true -> ID end),
    wf:set(device_msisdn, MSISDN),
    wf:set(device_status_value, Status),
    wf:wire("Nitrogen.$check('device_status_active'," ++
		atom_to_list(Status=:="active") ++");"),
    wf:wire("Nitrogen.$check('device_status_inactive'," ++
		atom_to_list(Status=:="inactive") ++ ");"),	    
    wf:wire("Nitrogen.$check('device_status_broken'," ++
		atom_to_list(Status=:="broken") ++ ");"),
    wf:wire("Nitrogen.$check('device_status_missing'," ++
		atom_to_list(Status=:="missing") ++ ");").

clear_dialog() ->
    wf:state(current_id, undefined),
    fill_dialog(undefined, "", "").

set_dialog(ID, Status, MSISDN) ->
    wf:state(current_id, ID),
    fill_dialog(ID, Status, MSISDN).


%% exodm_web_table callback
row_selected(I, ID) ->
    io:format("row_selected: row=~w, id=~p\n", [I, ID]),
    case exodm_db_device:read(?CURRENT_USER, ID) of
	{ok,Rec} ->
	    [{_,ID},{_,Status},{_,MSISDN}] = 
		maximus_db:format_record(Rec,[id,status,employee_id]),
	    io:format("update: id=~p, status=~p\n", [ID,Status]),
	    set_dialog(ID, Status, MSISDN);
	{error,Reason} ->
	    io:format("read error: card=~p : reason=~p\n", [ID,Reason]),
	    ok
    end.

event(copy) ->  %% create a new card from ID
    case wf:q(last_rfid_value) of
	undefined ->
	    io:format("COPY: ~p\n", [undefined]),
	    ok;
	ID ->
	    io:format("COPY: ~p\n", [ID]),
	    exodm_web_table:deselect(device),
	    wf:state(current_id, undefined),
	    fill_dialog(ID, "", "")
    end;
event({update,Element}) ->
    io:format("update: id=~w\n", [Element]),
    case Element of
	device_status_active ->
	    wf:set(device_status_value, "active");
	device_status_inactive ->
	    wf:set(device_status_value, "inactive");
	device_status_broken ->
	    wf:set(device_status_value, "broken");
	device_status_missing ->
	    wf:set(device_status_value, "missing");
	_ -> 
	    ok
    end,
    %% mark current record as dirty (need add/update)
    ok;
event(delete) ->
    delete_current();
event(add) ->
    add_or_update(add);
event(update) ->
    add_or_update(update);
event(Event) ->
    io:format("Event=~p~n",[Event]),
    ok.

%% add / update item or current item
%% FIXME: update table entry - if visible
add_or_update(Operation) ->
    ID = wf:q(device_id),
    Status = wf:q(device_status_value),
    MSISDN = wf:q(device_msisdn),

    io:format("~p: ID:~p, Status:~p, Msisdn:~p\n", 
	      [Operation,ID,Status, MSISDN]),

    case wf:state(current_id) of
	ID ->
	    case exodm_db_device:exist(?CURRENT_USER, ID) of
		true ->
		    exdm_db_device:update(?CURRENT_USER, ID,
					   [{status,Status},
					    {msisdn,MSISDN}]);
		false ->
		    throw({invalid,id,"update error"})
	    end;
	_ -> %% no card was loaded
	    case exodm_db_device:exist(?CURRENT_USER, ID) of
		true ->
		    exodm_db_device:update(?CURRENT_USER, ID,
					   [{status,Status},
					    {msisdn,MSISDN}]),
		    wf:state(current_id, undefined);
		false ->
		    exodm_db_device:new(?CURRENT_USER, ID,
					[{status,Status},
					 {msisdn,MSISDN}]),
		    wf:state(current_id, undefined);
		_Error ->
		    throw({invalid,id,"update error"})
	    end
    end.
    
%% delete current item (named current_id)
delete_current() ->
    case wf:state(current_id) of
	undefined ->
	    ok;  %% nothing to do
	ID ->
	    io:format("DELETE: ID=~p\n", [ID]),
	    case exodm_db_device:exist(ID) of
		true ->
		    %% confirm dialog? if modified? FIXME!!!
		    exodm_db_device:delete(ID),
		    exodm_web_table:delete_selected(device),
		    clear_dialog();
		_ ->
		    wf:state(current_id, undefined),
		    ok
	    end
    end.

%% This should subscribe to recent activity on device 
background_update_entry() ->
    %% {ok,{_Ts,Value}} = maximus_srv:last_rfid(),
    background_set("FIXME"),
    %% maximus_srv:subscribe([rfid]),
    background_update_loop().

background_update_loop() ->
    receive
	'INIT' ->
	    %% Nothing to do right now
	    background_update_loop();

	{maximus_event, rfid, _Ts, ID} ->
	    io:format("Got event: rfid, value=~p\n", [ID]),
	    background_set(ID),
	    background_update_loop();
	Other ->
	    io:format("Got event: ~p\n", [Other]),
	    background_update_loop()
    end.

background_set("") ->
    wf:set(last_rfid, "--------"),
    wf:set(last_rfid_value, undefined),
    wf:flush();
background_set(Value) ->
    wf:set(last_rfid, Value),
    wf:set(last_rfid_value, Value),
    wf:wire(last_rfid, [#fade{}, #appear{}]),
    wf:flush().
