%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2010, Tony Rogvall

-module(exodm_web_logout).

-export([main/0]).


-include_lib("nitrogen_core/include/wf.hrl").

main() -> 
    exodm_web_common:content_type_html(),
    wf:logout(),
    %% wf:clear_session(),
    %% wf:user(undefined),
    %% wf:session(authenticated, false),
    wf:redirect("/").


	
