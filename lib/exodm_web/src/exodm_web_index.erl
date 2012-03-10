%% @author Tony Rogvall tony@rogvall.se
%% @copyright YYYY Tony Rogvall.

-module(exodm_web_index).

-include_lib("nitrogen_core/include/wf.hrl").

-export([main/0
         , title/0
         , layout/0
	 , event/1
	]).

main() ->
    exodm_web_common:content_type_html(),
    File = filename:join([code:priv_dir(exodm_web),"templates","grid.html"]),
    #template { file=File }.

title() ->
    exodm_web_common:title().

layout() ->
    #container_12 {
        body=[#grid_12 { class=header, body=exodm_web_common:header(home) },
              #grid_clear {},

              #grid_6 { alpha=true, body=exodm_web_common:left() },
              #grid_6 { omega=true, body=exodm_web_common:right() },
              #grid_clear {},
              
              #grid_12 { body=exodm_web_common:footer() }
             ]}.

event(Event) ->
    io:format("Event=~p~n",[Event]),
    ok.
