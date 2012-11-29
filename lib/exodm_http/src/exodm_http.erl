-module(exodm_http).

-export([add_session/0,
	 add_session/1]).


add_session() ->
    exodm_http_server:add_session().

add_session(App) ->
    exodm_http_server:add_session(App).
