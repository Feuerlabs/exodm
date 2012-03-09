%% @author Tony Rogvall <tony@rogvall.se>
%% @copyright 2010 Tony Rogvall

-module(exodm_web_common).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("gettext/include/gettext.hrl").

-export([title/0
         , header/1
         , footer/0
         , right/0
         , left/0
        ]).
-export([content_type_html/0]).
-export([to_utf8/1]).

content_type_html() ->
    CharSet = 
	case gettext:lang2cset(get(gettext_language)) of
	    {ok, CSet} -> CSet;
	    _ -> "iso-8859-1"
	end,
    %% io:format("CharSet=~p\n", [CharSet]),
    wf:content_type("text/html; charset='"++CharSet++"'").

to_utf8(Chars) ->
    unicode:characters_to_binary(Chars, utf32, utf8).

title() ->
    "Exosense".

right() ->
    #panel { class=menu, body=[""] }.


left() ->
    #panel { class=menu, body=[""] }.

header(Selected) ->
    wf:wire(Selected, #add_class { class=selected }),
    #panel { class=menu, body=[
	#image { image="/images/fl_small.png", alt="Fuerlabs" },
        #link { id=home,     url='/',            text=?TXT("Home")     },
        #link { id=activity, url='/activity',    text=?TXT("Activity") },
        #link { id=device,   url='/devices',     text=?TXT("Devices")  },
        #link { id=group,    url='/groups',      text=?TXT("Groups")   },
        #link { id=logout,   url='/logout',      text=?TXT("Logout")   },
        #link { id=about,    url='/about',       text=?TXT("About")    }
    ]}.

footer() ->
    [#br{},
     #panel { class=credits, body=[
        "
        Copyright &copy; 2012 <a href='http://www.feuerlabs.com'>FeuerLabs</a>."
				  ]}].

			     
