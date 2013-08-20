%%% @author Malotte W Lönne <malotte@malotte.net>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     Exosense db definitions
%%% @end
%%% Created :  2013 Malotte W Lönne <malotte@malotte.net>

%% Account DB fields

-define(ACC_DB_ID,           <<"id">>). %% Dummy field, used when returning data
-define(ACC_DB_LAST_REQ,     <<"__last_req_id">>).
-define(ACC_DB_LAST_TID,     <<"__last_tid">>).
-define(ACC_DB_APPIDS,       <<"appids">>).
-define(ACC_DB_NAME,         <<"name">>).
-define(ACC_DB_ADMINS,       <<"admins">>).
-define(ACC_DB_ROLES,        <<"roles">>).
-define(ACC_DB_USERS,        <<"users">>).
-define(ACC_DB_GROUPS,       <<"groups">>).
-define(ACC_DB_PROTS,        <<"protocols">>).
-define(ACC_DB_SYSTEM_SPECS, <<"system_specs">>).
-define(ACC_DB_GROUP_NAME,   <<"name">>).

%% Account options 
-define(ACC_OPT_NAME, name).
-define(ACC_OPT_ROOT, root).

%% Role DB fields
-define(ROLE_DB_DESCR,  <<"descr">>).
-define(ROLE_DB_EXISTS, <<"exists">>).
-define(ROLE_DB_ACCESS, <<"access">>).

%% Role options
-define(ROLE_OPT_DESCR,  descr).
-define(ROLE_OPT_ACCESS, access). %% Should be replaced by rpc:s???

%% Device
-define(DEV_DB_CONFIG_SET,      <<"config_set">>).
-define(DEV_DB_DEVICE_TYPE,     <<"device-type">>).
-define(DEV_DB_PROTOCOL,        <<"protocol">>).
-define(DEV_DB_PUSH_PROTOCOL,   <<"push-protocol">>).
-define(DEV_DB_DEVICE_ID,       <<"device-id">>).
-define(DEV_DB_LATITUDE,        <<"latitude">>).
-define(DEV_DB_LONGITUDE,       <<"longitude">>).
-define(DEV_DB_TIMESTAMP,       <<"timestamp">>).
-define(DEV_DB_DEVICE_KEY,      <<"device-key">>).
-define(DEV_DB_SERVER_KEY,      <<"server-key">>).
-define(DEV_DB_SESSION_TIMEOUT, <<"session-timeout">>).
-define(DEV_DB_GROUPS,          <<"groups">>).
-define(DEV_DB_PASSWORD,        <<"password">>).
-define(DEV_DB_REQ_URLS,        <<"request-urls">>).

%% Users
-define(USER_DB_NAME,     <<"name">>).
-define(USER_DB_FULLNAME, <<"fullname">>).
-define(USER_DB_PHONE,    <<"phone">>).
-define(USER_DB_EMAIL,    <<"email">>).
-define(USER_DB_SKYPE,    <<"skype">>).
-define(USER_DB_PASSWORD, <<"password">>).
-define(USER_DB_ALIAS,    <<"__alias">>). %% Why not <<"alias">> ?? FIXME

%% Role field should be renamed in database FIXME (any day)
-define(USER_ROLE,        <<"access">>).
-define(USER_DB_ROLE_AID, <<"__aid">>).
-define(USER_DB_ROLE_RID, <<"__rid">>).

%% User options
-define(USER_OPT_NAME,     name). 
-define(USER_OPT_FNAME,    fullname). 
-define(USER_OPT_PASSWORD, password). 
-define(USER_OPT_PHONE,    phone). 
-define(USER_OPT_EMAIL,    email). 
-define(USER_OPT_SKYPE,    skype). 
-define(USER_OPT_ALIAS,    alias). 

