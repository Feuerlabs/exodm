%%% @author Malotte W Lönne <malotte@malotte.net>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense db definitions
%%% @end
%%% Created :  2013 Malotte W Lönne <malotte@malotte.net>

%% Account DB fields

-define(ACC_DB_ID, <<"id">>). %% Dummy field, used when returning data
-define(ACC_DB_LAST_REQ, <<"__last_req_id">>).
-define(ACC_DB_LAST_TID, <<"__last_tid">>).
-define(ACC_DB_NAME, <<"name">>).
-define(ACC_DB_ADMINS, <<"admins">>).
-define(ACC_DB_ROLES, <<"roles">>).
-define(ACC_DB_USERS, <<"users">>).
-define(ACC_DB_GROUPS, <<"groups">>).
-define(ACC_DB_PROTS, <<"protocols">>).
-define(ACC_DB_SYSTEM_SPECS, <<"system_specs">>).
-define(ACC_DB_GROUP_NAME, <<"name">>).

%% Account options 
-define(ACC_OPT_NAME, name).
-define(ACC_OPT_ROOT, root).
-define(ACC_OPT_ADMIN, admin).
-define(ACC_OPT_UNAME, uname). %% Should be defined in user
-define(ACC_OPT_FNAME, fullname). %% Should be defined in user
-define(ACC_OPT_PWD, password). %% Should be defined in user

%% Role DB fields
-define(ROLE_DB_DESCR, <<"descr">>).
-define(ROLE_DB_EXISTS, <<"exists">>).
-define(ROLE_DB_ACCESS, <<"access">>).

%% Role options
-define(ROLE_OPT_DESCR, descr).
-define(ROLE_OPT_ACCESS, access). %% Should be replaced by rpc:s???
