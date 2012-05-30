%% @author Tony Rogvall tony@rogvall.se
%% @copyright YYYY Tony Rogvall.

%% @doc The initiating leg of the OpenID authentication.

-module(exodm_web_register).

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
              #grid_4 { class=dialog, body=registration_form()},
              #grid_4 { body=[] },
              #grid_clear {},
              #grid_4 { body=exodm_web_common:footer() }
             ]}.


registration_form() ->
%%    wf:wire(okButton, userTextBox, 
%%	    #validate { validators= [ #is_required { text="Required" } ]}),
%%    wf:wire(okButton, passTextBox, 
%%	    #validate { validators=
%%			    [ #is_required { text="Required" }
%%			      #custom { text="Hint: password is 'secret'.", 
%%					function=fun check_password/2 }
%%			    ]}),    
      Panel =
	#panel {
	body=[
	      #label { text="Username" },
	      #textbox { id=userTextBox, next=passTextBox },
	      #p{},
	      #label { text="Password" },
	      #password { id=passTextBox, next=okButton },
	      #label { text="Verify Password" },
	      #password {id=passTextBox2, next=fullNameTextBox },
	      #label { text="Full Name"},
	      #textbox { id=fullNameTextBox, next=emailTextBox },
	      #label { text="Email"},
	      #textbox { id=emailTextBox, next=emailTextBox2 },
	      #label { text="Repeat Email"},
	      #textbox { id=emailTextBox2, next=phoneTextBox },
	      #label { text="Phone"},
	      #textbox { id=phoneTextBox, next=skypeTextBox },
	      #label { text="Skype"},
	      #textbox { id=skypeTextBox, next=regButton },
	      #p{},
	      #button { id=regButton, text="Register", postback=register }]},
    %% validators
    wf:wire(regButton, userTextBox,
	    #validate{validators=
			  [#is_required{text="Required."},
			   #custom{text=usertaken(),
				   tag=some_tag,
				   function=fun username_free/2}
			  ]}),
    wf:wire(regButton, passTextBox,
	    #validate{validators=
			  [#is_required{text="Required."},
			   #min_length{length=6,
				       text=pwdlen()}] }),
    wf:wire(regButton, emailTextBox,
	    #validate{validators=
			  [#is_required { text="Required." },
			   #is_email { text=validemail() }
			  ]}),
    wf:wire(regButton, emailTextBox2,
	    #validate{validators=
			  [#is_required { text="Required." },
			   #is_email { text=emailmatch() }
			  ]}),
    wf:wire(regButton, passTextBox2,
	    #validate{validators=
			  [#is_required{text="Required." },
			   #confirm_password{
					 password=passTextBox2,
					 text=pwdmatch()
					}] }),
    Panel.


usertaken() -> "The user name already exists.".
pwdlen() -> "The password must consist of at least 8 characters.".
pwdmatch() -> "The passwords must match.".
validemail() -> "Enter a valid email address.".
emailmatch() -> "The email addresses must match.".

%% check_password(_Tag, Value) ->
%%     User = wf:q(userTextBox),
%%     Value =:= "tesla".

event(register) ->
    [User, Password, FullName, Email, Phone, Skype] =
	[wf:q(Fld) || Fld <- [userTextBox, passTextBox,
			      fullNameTextBox, emailTextBox,
			      phoneTextBox, skypeTextBox]],
    Info = [{'__password', l2b(Password)},
	    {fullname, l2b(FullName)},
	    {email, l2b(Email)},
	    {phone, l2b(Phone)},
	    {skype, l2b(Skype)}],
    exodm_db_user:new(l2b(User), Info),
    wf:redirect_from_login("/");
event(_Event) ->
    io:format("Event = ~p\n", [_Event]).


username_free(_Tag, User) ->
    not exodm_db_user:exist(0, list_to_binary(User)).

l2b(X) ->
    exodm_db:to_binary(X).
