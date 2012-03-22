-module(exodm_web_yaws).
-export ([out/1]).

-include_lib("yaws/include/yaws_api.hrl").
-import(lists, [reverse/1]).

%%% @doc This is the routing table.
routes() ->
    [{"/",            exodm_web_index}
     , {"/login",     exodm_web_login}
     , {"/logout",    exodm_web_logout}
     , {"/location",  exodm_web_location}
     , {"/activity",  exodm_web_activity}
     , {"/auth",      exodm_web_auth}
     , {"/ajax",      exodm_web_ajax}
     , {"/device",    exodm_web_device}
     , {"/group",     exodm_web_group}
     , {"/nitrogen",  static_file}
     , {"/images",    static_file}
     , {"/js",        static_file}
     , {"/css",       static_file}
    ].

%%
%% This code handles large partial post data (non-multipart)
%% This can be used to scan trough big chunks of data
%% - warn when post data is too big
%% - flush post data and present error messages
%% - kill of connection in a application friendly way.
%%
out(Info) ->
    H = Info#arg.headers,
    R = Info#arg.req,
    case R#http_request.method of
	'POST' ->
	    case H#headers.content_type of
		"multipart/form-data" ++ _ ->
		    out_(Info);
		_CType when Info#arg.cont =:= undefined ->
		    case Info#arg.clidata of
			{partial,Data} ->
			    case Info#arg.state of
				undefined -> 
				    {get_more, undefined, [Data]};
				Ds ->
				    {get_more, undefined, [Data|Ds]}
			    end;
			Data ->
			    case Info#arg.state of
				undefined ->
				    out_(Info);
				Ds when is_list(Ds) ->
				    Data1=list_to_binary(reverse([Data|Ds])),
				    out_(Info#arg { clidata=Data1, 
						    state=undefined })
			    end
		    end;
		_CType ->
		    out_(Info)
	    end;
	_Method ->
	    out_(Info)
    end.

out_(Info) ->
    put(gettext_language, "en"),
    RequestBridge = simple_bridge:make_request(yaws_request_bridge, Info),
    ResponseBridge = simple_bridge:make_response(yaws_response_bridge, Info),
    nitrogen:init_request(RequestBridge, ResponseBridge),
    replace_route_handler(),
    nitrogen:run().

replace_route_handler() ->
    wf_handler:set_handler(named_route_handler, routes()).

