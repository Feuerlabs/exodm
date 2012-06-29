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
        body=[#grid_12 { class=header, body=exodm_web_common:header(device) },
              #grid_clear {},

              #grid_6 { alpha=true, body=device_table() },
              #grid_6 { omega=true, body=
			    [#panel { class=dialog,body=device_id_entry()},
			     #br{},
			     #panel { class=dialog,body=device_dialog() },
			     #panel { class=dialog,body=device_location() }
			    ]},
              #grid_clear {},
              #grid_12 { body=exodm_web_common:footer() }
             ]}.

format_row(header) ->
    [?TXT("Device"), ?TXT("Name"), ?TXT("MSISDN")];
format_row(Row) ->
    [{id,proplists:get_value(id,Row,"?")},
     {name,proplists:get_value(name,Row,"?")},
     {msisdn,proplists:get_value(msisdn,Row,"")}
    ].

format_device_id("?") ->
    "?";
format_device_id(ID) ->
    exodm_db:decode_id(ID).


device_table() ->
    wf:state(current_id, undefined),
    AID = wf:session(account_id),
    {Tab, Key} = exodm_db_device:tab_and_key(AID),
    exodm_web_table:layout(device, Tab, Key, ?MODULE).

device_id_entry() ->
    wf:comet(fun() -> background_update_entry() end),
    Actions = #event { type=click, postback=copy },
    [
     #grid_clear {},
     #grid_2 { body=[#label    { text=?TXT("Activity ID")},
		     #hidden   { id=last_device_id_value },
		     #singlerow { cells=
				      [#tablecell { id=last_device_id,
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
				 text=?TXT("Inactive") },
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
     #label { text=?TXT("Device") }, device_id(""),
     #label { text=?TXT("Name") },   device_name(""),
     #label { text=?TXT("MSISDN") }, device_msisdn(""),
     #label { text=?TXT("IMSI") },   device_imsi(""),
     #label { text=?TXT("LAT") },    device_lat(""),
     #label { text=?TXT("LONG") },   device_long(""),
     %% Show group dropdown?
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

get_current_id() ->
    case wf:state(current_id) of
	undefined ->
	    io:format("current_id: undefined\n"),
	    undefined;
	DID ->
	    io:format("current_id: ~w\n", [DID]),
	    DID
    end.

get_location_url(DID) ->
    AID = wf:session(account_id),    
    case exodm_db_device:lookup_position(AID, DID) of
	{0,0,0} ->
	    <<"">>;
	{Lat,Lon,_Ts} ->
	    LatF = io_lib:format("~.6.0f",[(Lat/100000.0) - 90]),
	    LonF = io_lib:format("~.6.0f",[(Lon/100000.0) - 180]),
	    list_to_binary(
	      ["http://maps.googleapis.com/maps/api/staticmap?"
	       "center=",LatF,",",LonF,
	       "&markers=color:red%7C",LatF,",",LonF,
	       "&zoom=14"
	       "&size=400x200"
	       "&sensor=true"
	      ])
    end.

%% experimental !
device_location() ->
    URL =
	case get_current_id() of
	    undefined -> 
		<<"">>;
	    DID ->
		get_location_url(DID)
	end,
    io:format("URL='~s'\n", [URL]),
    #image { id=device_map_url, image=URL }.
%%    [
%%     "<div>",
%%     "<iframe width='400' height='200' frameborder='0' scrolling='no' marginheight='0' marginwidth='0' src='", URL, "'>",
%%     "</iframe></div>"].


device_id(Value) ->
    #textbox { id=device_id, text=Value,
	       postback={update,device_id} }.

device_name(Value) ->
    #textbox { id=device_name, text=Value,
	       postback={update,device_name} }.

device_msisdn(Value) ->
    #textbox { id=device_msisdn, text=Value, 
	       postback={update,device_msisdn} }.

device_imsi(Value) ->
    #textbox { id=device_imsi, text=Value, 
	       postback={update,device_imsi} }.

device_long(Value) ->
    #textbox { id=device_long, text=Value, 
	       postback={update,device_long} }.

device_lat(Value) ->
    #textbox { id=device_lat, text=Value, 
	       postback={update,device_lat} }.

fill_dialog(ID, Data) ->
    wf:set(device_id, if ID =:= undefined -> <<"">>; true -> ID end),
    wf:set(device_name, proplists:get_value(name, Data, <<"">>)),
    wf:set(device_msisdn,proplists:get_value(msisdn, Data, <<"">>)),
    wf:set(device_imsi,proplists:get_value(imsi,Data, <<"">>)),
    wf:set(device_long,proplists:get_value(longitude,Data, <<"">>)),
    wf:set(device_lat,proplists:get_value(latitude,Data,<<"">>)),
    wf:set(device_map_url, proplists:get_value(url,Data,<<"">>)),
    Status = proplists:get_value(status,Data,<<"active">>),
    wf:set(device_status_value,Status),
    wf:wire("Nitrogen.$check('device_status_active'," ++
		atom_to_list(Status=:=<<"active">>) ++");"),
    wf:wire("Nitrogen.$check('device_status_inactive'," ++
		atom_to_list(Status=:=<<"inactive">>) ++ ");"),	    
    wf:wire("Nitrogen.$check('device_status_broken'," ++
		atom_to_list(Status=:=<<"broken">>) ++ ");"),
    wf:wire("Nitrogen.$check('device_status_missing'," ++
		atom_to_list(Status=:=<<"missing">>) ++ ");").

clear_dialog() ->
    wf:state(current_id, undefined),
    fill_dialog(undefined, []).


%% exodm_web_table callback
row_selected(I, ID) ->
    io:format("row_selected: row=~w, id=~p\n", [I, ID]),
    DID = exodm_db:encode_id(ID),
    AID = wf:session(account_id),
    case exodm_db_device:lookup(AID, DID) of
	[] ->
	    io:format("could not find device (~w, ~w)\n", 
		      [AID, DID]),
	    ok;
	DeviceData ->
	    Url = get_location_url(DID),
	    wf:state(current_id, DID),
	    fill_dialog(ID,[{url,Url}|DeviceData])
    end.


event(copy) ->  %% create a new device from ID
    case wf:q(last_device_id_value) of
	undefined ->
	    io:format("COPY: ~p\n", [undefined]),
	    ok;
	ID ->
	    io:format("COPY: ~p\n", [ID]),
	    exodm_web_table:deselect(device),
	    wf:state(current_id, undefined),
	    fill_dialog(ID, [])
    end;
event({update,Element}) ->
    %% FIXME: refresh 
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
    ID     = list_to_integer(wf:q(device_id)),
    MSISDN = wf:q(device_msisdn),
    Long   = list_to_integer(wf:q(device_long)),
    Lat    = list_to_integer(wf:q(device_lat)),
    %% FIXME: add rest of the data in correct format!
    Data   = [{msisdn,MSISDN},{longitude,Long},{latitude,Lat}],
    io:format("~p: ID=~p, Data:~p\n", [ID, Operation,Data]),

    case wf:state(current_id) of
	ID ->
	    AID = wf:session(account_id),
	    case exodm_db_device:exist(AID, ID) of
		true ->
		    exdm_db_device:update(AID, ID, Data);
		false ->
		    throw({invalid,id,"update error"})
	    end;
	_ -> %% no device was loaded
	    AID = wf:session(account_id),
	    case exodm_db_device:exist(AID, ID) of
		true ->
		    exodm_db_device:update(AID, ID, Data),
		    wf:state(current_id, undefined);
		false ->
		    exodm_db_device:new(AID, ID, Data),
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
	    AID = wf:session(account_id),
	    case exodm_db_device:exist(AID,ID) of
		true ->
		    %% confirm dialog? if modified? FIXME!!!
		    exodm_db_device:delete(AID,ID),
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

	{device_event, id, _Ts, ID} ->
	    io:format("Got event: id, value=~p\n", [ID]),
	    background_set(ID),
	    background_update_loop();
	Other ->
	    io:format("Got event: ~p\n", [Other]),
	    background_update_loop()
    end.

background_set("") ->
    wf:set(last_device_id, "--------"),
    wf:set(last_device_id_value, undefined),
    wf:flush();
background_set(Value) ->
    wf:set(last_device_id, Value),
    wf:set(last_device_id_value, Value),
    wf:wire(last_device_id, [#fade{}, #appear{}]),
    wf:flush().
