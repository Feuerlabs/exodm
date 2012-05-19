%% @author Tony Rogvall tony@rogvall.se
%% @copyright YYYY Tony Rogvall.

%% @doc The initiating leg of the OpenID authentication.

-module(exodm_web_login).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("gettext/include/gettext.hrl").

-export([main/0,
         title/0,
         layout/0,
	 event/1
	]).

-define(GA_ACCOUNT_ID, 1).

main() ->
    exodm_web_common:content_type_html(),
    File = filename:join([code:priv_dir(exodm_web),
			  "templates","login.html"]),
    #template { file=File }.

    
title() ->
    exodm_web_common:title().

layout() ->
    #container_12 { 
        body=[
        %%    #grid_12 { class=header,body=exodm_web_common:header(login) },
	%%      #grid_clear {},
	      #image { image="/images/fl_small.png", alt="Feuerlabs" },
              #grid_4 { body=[] },
              #grid_4 { class=dialog, body=login_form()},
              #grid_4 { body=[] },
              #grid_clear {},
              #grid_4 { body=exodm_web_common:footer() }
             ]}.


login_form() ->
%%    wf:wire(okButton, userTextBox, 
%%	    #validate { validators= [ #is_required { text="Required" } ]}),
%%    wf:wire(okButton, passTextBox, 
%%	    #validate { validators=
%%			    [ #is_required { text="Required" }
%%			      #custom { text="Hint: password is 'secret'.", 
%%					function=fun check_password/2 }
%%			    ]}),    
      #panel { 
	 body=[
	       #label { text="Username" },
	       #textbox { id=userTextBox, next=passTextBox },
	       #p{},
	       #label { text="Password" },
	       #password { id=passTextBox, next=okButton },
	       #p{},
	       #button { id=okButton, text="OK", postback=ok }
	      ]}.

    
%% check_password(_Tag, Value) ->
%%     User = wf:q(userTextBox),
%%     Value =:= "tesla".

event(ok) ->
    User = wf:q(userTextBox),
    Password = wf:q(passTextBox),
    io:format("user=~p, password=~p\n", [User, Password]),
    case User of
	"admin" ->
	    if Password =:= "tesla" ->
		    wf:user(admin),
		    wf:role(managers, true),
		    wf:session(account_id, ?GA_ACCOUNT_ID),
		    wf:redirect_from_login("/");
	       true ->
		    wf:flash(?TXT("Invalid user or password."))
	    end;
	_ ->
	    wf:flash(?TXT("Invalid user or password."))
    end;
event(_Event) ->
    io:format("Event = ~p\n", [_Event]).


